/*
*
This file is a heavily modified version of tea.go as it appear{s,ed} in
https://github.com/charmbracelet/bubbletea with the following LICENSE:

MIT License

# Copyright (c) 2020-2023 Charmbracelet, Inc

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

package latte

import (
	"context"
	"errors"
	"io"
	"sync"
	"time"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/util"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/muesli/cancelreader"
	"github.com/sasha-s/go-deadlock"
	"golang.org/x/sync/errgroup"
)

type Cmd = tea.Cmd
type Msg = tea.Msg

// ErrProgramKilled is returned by [Program.Run] when the program got killed.
var ErrProgramKilled = errors.New("program was killed")

// Model contains the program's state as well as its core functions.
type Model interface {
	// Init is the first function that will be called. It returns an optional
	// initial command. To not perform an initial command return nil.
	Init() tea.Cmd

	// Update is called when a message is received. Use it to inspect messages
	// and, in response, update the model and/or send a command.
	Update(tea.Msg) (Model, tea.Cmd)

	// View renders the program's UI, which consists of a full *tty.State. The
	// view is rendered after every Update.
	View(*tty.State)
}

type handlers []chan struct{}

// Program is a terminal user interface.
type Program struct {
	util.Lifetime
	renderer

	initialModel Model

	msgs     chan tea.Msg
	errs     chan error
	finished chan struct{}

	// where to read inputs from, this will usually be os.Stdin.
	writes       *io.PipeWriter
	input        io.Reader
	cancelReader cancelreader.CancelReader
	readLoopDone chan struct{}
}

var _ mux.Screen = (*Program)(nil)

func (p *Program) Resize(size geom.Size) error {
	p.renderer.resize(size)
	p.Send(tea.WindowSizeMsg{
		Width:  size.C,
		Height: size.R,
	})
	return nil
}

// Sequence runs the given commands one at a time, in order. Contrast this with
// Batch, which runs commands concurrently.
func Sequence(cmds ...tea.Cmd) tea.Cmd {
	return func() tea.Msg {
		return sequenceMsg(cmds)
	}
}

// sequenceMsg is used internally to run the given commands in order.
type sequenceMsg []tea.Cmd

// NewProgram creates a new Program.
func NewProgram(model Model) *Program {
	p := &Program{
		initialModel: model,
		msgs:         make(chan tea.Msg),
	}

	return p
}

func (p *Program) Write(data []byte) (n int, err error) {
	return p.writes.Write(data)
}

// handleCommands runs commands in a goroutine and sends the result to the
// program's message channel.
func (p *Program) handleCommands(cmds chan tea.Cmd) chan struct{} {
	ch := make(chan struct{})

	go func() {
		defer close(ch)

		for {
			select {
			case <-p.Ctx().Done():
				return

			case cmd := <-cmds:
				if cmd == nil {
					continue
				}

				// Don't wait on these goroutines, otherwise the shutdown
				// latency would get too large as a Cmd can run for some time
				// (e.g. tick commands that sleep for half a second). It's not
				// possible to cancel them so we'll have to leak the goroutine
				// until Cmd returns.
				go func() {
					msg := cmd() // this can be long.
					p.Send(msg)
				}()
			}
		}
	}()

	return ch
}

func (p *Program) render(model Model) {
	p.renderer.Lock()
	model.View(p.renderer.state)
	p.renderer.Unlock()
}

// eventLoop is the central message loop. It receives and handles the default
// Bubble Tea messages, update the model and triggers redraws.
func (p *Program) eventLoop(model Model, cmds chan tea.Cmd) (Model, error) {
	for {
		select {
		case <-p.Ctx().Done():
			return model, nil

		case err := <-p.errs:
			return model, err

		case msg := <-p.msgs:
			if msg == nil {
				continue
			}

			// Handle special internal messages.
			switch msg := msg.(type) {
			case tea.QuitMsg:
				return model, nil

			case tea.BatchMsg:
				for _, cmd := range msg {
					cmds <- cmd
				}
				continue

			case sequenceMsg:
				go func() {
					// Execute commands one at a time, in order.
					for _, cmd := range msg {
						if cmd == nil {
							continue
						}

						msg := cmd()
						if batchMsg, ok := msg.(tea.BatchMsg); ok {
							g, _ := errgroup.WithContext(p.Ctx())
							for _, cmd := range batchMsg {
								cmd := cmd
								g.Go(func() error {
									p.Send(cmd())
									return nil
								})
							}

							//nolint:errcheck
							g.Wait() // wait for all commands from batch msg to finish
							continue
						}

						p.Send(msg)
					}
				}()
			}

			var cmd tea.Cmd
			model, cmd = model.Update(msg) // run update
			cmds <- cmd                    // process command (if any)
			p.renderer.write(model)        // send view to renderer
		}
	}
}

// initCancelReader (re)commences reading inputs.
func (p *Program) initCancelReader() error {
	var err error
	p.cancelReader, err = cancelreader.NewReader(p.input)
	if err != nil {
		return err
	}

	p.readLoopDone = make(chan struct{})
	go p.readLoop()

	return nil
}

func (p *Program) readLoop() {
	defer close(p.readLoopDone)

	for {
		if p.Ctx().Err() != nil {
			return
		}

		msgs, err := readInputs(p.cancelReader)
		if err != nil {
			if !errors.Is(err, io.EOF) && !errors.Is(err, cancelreader.ErrCanceled) {
				select {
				case <-p.Ctx().Done():
				case p.errs <- err:
				}
			}

			return
		}

		for _, msg := range msgs {
			p.msgs <- msg
		}
	}
}

// waitForReadLoop waits for the cancelReader to finish its read loop.
func (p *Program) waitForReadLoop() {
	select {
	case <-p.readLoopDone:
	case <-time.After(500 * time.Millisecond):
		// The read loop hangs, which means the input
		// cancelReader's cancel function has returned true even
		// though it was not able to cancel the read.
	}
}

// Run initializes the program and runs its event loops, blocking until it gets
// terminated by either [Program.Quit], [Program.Kill], or its signal handler.
// Returns the final model.
func (p *Program) Run() (Model, error) {
	handlers := handlers{}
	cmds := make(chan tea.Cmd)
	p.errs = make(chan error)
	p.finished = make(chan struct{}, 1)

	defer p.Cancel()

	// Initialize the program.
	model := p.initialModel
	if initCmd := model.Init(); initCmd != nil {
		ch := make(chan struct{})
		handlers.add(ch)

		go func() {
			defer close(ch)

			select {
			case cmds <- initCmd:
			case <-p.Ctx().Done():
			}
		}()
	}

	// Render the initial view.
	p.renderer.write(model)

	// Subscribe to user input.
	if p.input != nil {
		if err := p.initCancelReader(); err != nil {
			return model, err
		}
	}

	// Process commands.
	handlers.add(p.handleCommands(cmds))

	// Run event loop, handle updates and draw.
	model, err := p.eventLoop(model, cmds)
	killed := p.Ctx().Err() != nil
	if killed {
		err = ErrProgramKilled
	} else {
		// Ensure we rendered the final state of the model.
		p.renderer.write(model)
	}

	// Tear down.
	p.Cancel()

	// Check if the cancel reader has been setup before waiting and closing.
	if p.cancelReader != nil {
		// Wait for input loop to finish.
		if p.cancelReader.Cancel() {
			p.waitForReadLoop()
		}
		_ = p.cancelReader.Close()
	}

	// Wait for all handlers to finish.
	handlers.shutdown()

	// Restore terminal state.
	p.shutdown(killed)

	return model, err
}

// Send sends a message to the main update function, effectively allowing
// messages to be injected from outside the program for interoperability
// purposes.
//
// If the program hasn't started yet this will be a blocking operation.
// If the program has already been terminated this will be a no-op, so it's safe
// to send messages after the program has exited.
func (p *Program) Send(msg tea.Msg) {
	select {
	case <-p.Ctx().Done():
	case p.msgs <- msg:
	}
}

// Quit is a convenience function for quitting Bubble Tea programs. Use it
// when you need to shut down a Bubble Tea program from the outside.
//
// If you wish to quit from within a Bubble Tea program use the Quit command.
//
// If the program is not running this will be a no-op, so it's safe to call
// if the program is unstarted or has already exited.
func (p *Program) Quit() {
	p.Send(tea.Quit())
}

// Kill stops the program immediately and restores the former terminal state.
// The final render that you would normally see when quitting will be skipped.
// [program.Run] returns a [ErrProgramKilled] error.
func (p *Program) Kill() {
	p.Cancel()
}

// Wait waits/blocks until the underlying Program finished shutting down.
func (p *Program) Wait() {
	<-p.finished
}

// shutdown performs operations to free up resources and restore the terminal
// to its original state.
func (p *Program) shutdown(kill bool) {
	p.finished <- struct{}{}
}

// Adds a handler to the list of handlers. We wait for all handlers to terminate
// gracefully on shutdown.
func (h *handlers) add(ch chan struct{}) {
	*h = append(*h, ch)
}

// Shutdown waits for all handlers to terminate.
func (h handlers) shutdown() {
	var wg sync.WaitGroup
	for _, ch := range h {
		wg.Add(1)
		go func(ch chan struct{}) {
			<-ch
			wg.Done()
		}(ch)
	}
	wg.Wait()
}

func New(ctx context.Context, model Model) *Program {
	in, writes := io.Pipe()

	p := &Program{
		Lifetime: util.NewLifetime(ctx),
		renderer: renderer{
			state:   tty.New(geom.DEFAULT_SIZE),
			changes: mux.NewPublisher(),
		},
		initialModel: model,
		msgs:         make(chan tea.Msg),
		writes:       writes,
		input:        in,
	}

	go p.Run()

	return p
}

type renderer struct {
	deadlock.RWMutex
	state   *tty.State
	changes *mux.UpdatePublisher
}

func (r *renderer) resize(size geom.Size) {
	r.Lock()
	r.state = tty.New(size)
	r.Unlock()
}

func (r *renderer) write(model Model) {
	r.Lock()
	r.state = tty.New(r.state.Image.Size())
	model.View(r.state)
	r.Unlock()
	r.changes.Publish(r.state)
}

func (r *renderer) State() *tty.State {
	r.RLock()
	defer r.RUnlock()
	return r.state
}

func (r *renderer) Updates() *mux.Updater {
	return r.changes.Subscribe()
}
