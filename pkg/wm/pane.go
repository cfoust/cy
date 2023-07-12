package wm

import (
	"context"
	"fmt"
	"io"
	"time"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/pty"
	"github.com/cfoust/cy/pkg/session"
	"github.com/cfoust/cy/pkg/util"

	"github.com/sasha-s/go-deadlock"
)

type PaneOptions struct {
	Directory string
	Command   string
	Args      []string
}

type PaneStatus int

const (
	PaneStatusStarting PaneStatus = iota
	PaneStatusHealthy
	PaneStatusFailed
)

// A virtual terminal consisting of a program that is always restarted.
type Pane struct {
	util.Lifetime
	deadlock.RWMutex
	*metaData

	status PaneStatus

	context PaneOptions
	session *session.Session
	pty     *pty.Pty
	changes *util.Publisher[time.Time]

	Terminal emu.Terminal
}

var _ Node = (*Pane)(nil)

// Subscribe to state updates.
func (p *Pane) Subscribe() *util.Subscriber[time.Time] {
	return p.changes.Subscribe()
}

func (p *Pane) GetSize() Size {
	p.Terminal.Lock()
	cols, rows := p.Terminal.Size()
	p.Terminal.Unlock()

	return Size{
		Rows:    rows,
		Columns: cols,
	}
}

func (p *Pane) notifyChange() {
	p.changes.Publish(time.Now())
}

func (p *Pane) Resize(size Size) {
	p.Terminal.Resize(size.Columns, size.Rows)
	// TODO(cfoust): 07/12/23 Make pane thread safe
	if p.pty != nil {
		p.pty.Resize(size.Rows, size.Columns)
	}
	p.notifyChange()
}

func (p *Pane) GetStatus() PaneStatus {
	p.RLock()
	status := p.status
	p.RUnlock()
	return status
}

func (p *Pane) Path() (string, error) {
	p.RLock()
	defer p.RUnlock()

	if p.pty == nil {
		return "", nil
	}

	return p.pty.Path()
}

func (p *Pane) setStatus(status PaneStatus) {
	p.Lock()
	p.status = status
	p.Unlock()
}

func (p *Pane) pollIO() error {
	buffer := make([]byte, 4096)

	for {
		numBytes, err := p.pty.Read(buffer)
		if err == io.EOF {
			return nil
		}
		if err != nil {
			// TODO(cfoust): 05/17/23
			return err
		}
		if numBytes == 0 {
			continue
		}

		copied := make([]byte, numBytes)
		copy(copied, buffer[:numBytes])

		p.session.Output(copied)
		_, err = p.Terminal.Write(copied)
		if err != nil {
			return err
		}

		// Let any clients know that this pane changed
		p.notifyChange()
	}
}

// Run the pane's command and only return when it's finished.
func (p *Pane) run() error {
	started := make(chan error)
	context := p.context

	go func() {
		p.setStatus(PaneStatusStarting)

		cols, rows := p.Terminal.Size()
		pty, err := pty.Run(
			p.Ctx(),
			context.Command,
			context.Args,
			context.Directory,
			rows,
			cols,
		)
		if err != nil {
			started <- err
		}

		p.pty = pty

		go p.pollIO()

		started <- nil
		p.setStatus(PaneStatusHealthy)
	}()

	err := <-started
	if err != nil {
		return err
	}

	return p.pty.Wait()
}

func (p *Pane) Write(data []byte) (n int, err error) {
	p.session.Input(data)
	return p.pty.Write(data)
}

const (
	SPIN_NUM_TRIES = 3
	SPIN_THRESHOLD = 1 * time.Second
)

// Run the pane's pty command over and over (exiting a shell or an editor will
// just start it again.) If it fails more than SPIN_NUM_TRIES in under
// SPIN_THRESHOLD, don't restart it.
func (p *Pane) spin() {
	errChan := make(chan error)

	startTime := time.Now()
	numErrors := 0

	for {
		go func() {
			errChan <- p.run()
		}()

		select {
		case <-p.Ctx().Done():
			return
		case err := <-errChan:
			if err == nil {
				continue
			}

			elapsed := time.Now().Sub(startTime)

			// Reset the clock if it's been a while since the last error
			// This is just to guard against commands that suddenly stop working
			if elapsed > SPIN_THRESHOLD {
				numErrors = 0
				startTime = time.Now()
			}

			numErrors += 1

			if elapsed < SPIN_THRESHOLD && numErrors == SPIN_NUM_TRIES {
				p.setStatus(PaneStatusFailed)
				p.Terminal.Write([]byte(fmt.Sprintf(
					"\ncommand '%s' failed, backing off",
					p.context.Command,
				)))
				return
			}
		}
	}
}

func newPane(ctx context.Context, context PaneOptions, size Size) *Pane {
	pane := Pane{
		Lifetime: util.NewLifetime(ctx),
		status:   PaneStatusStarting,
		context:  context,
		session:  session.New(),

		Terminal: emu.New(emu.WithSize(
			size.Columns,
			size.Rows,
		)),

		changes: util.NewPublisher[time.Time](),
	}

	pane.session.Resize(size.Columns, size.Rows)

	go pane.spin()

	return &pane
}
