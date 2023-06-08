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

type Size struct {
	Rows    int
	Columns int
}

var DEFAULT_SIZE = Size{
	Rows:    26,
	Columns: 80,
}

type PaneContext struct {
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

	status PaneStatus

	context PaneContext
	session *session.Session
	pty     *pty.Pty

	Terminal emu.Terminal
}

func (p *Pane) GetStatus() PaneStatus {
	p.RLock()
	status := p.status
	p.RUnlock()
	return status
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
	}
}

// Run the pane's command and only return when it's finished.
func (p *Pane) run() error {
	started := make(chan error)
	context := p.context

	go func() {
		p.setStatus(PaneStatusStarting)

		pty, err := pty.Run(
			p.Ctx(),
			context.Command,
			context.Args,
			context.Directory,
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

func NewPane(ctx context.Context, context PaneContext, size Size) *Pane {
	pane := Pane{
		Lifetime: util.NewLifetime(ctx),
		status:   PaneStatusStarting,
		context:  context,
		Terminal: emu.New(emu.WithSize(
			size.Columns,
			size.Rows,
		)),
		session: session.New(),
	}

	pane.session.Resize(size.Columns, size.Rows)

	go pane.spin()

	return &pane
}
