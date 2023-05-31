package cy

import (
	"context"
	"io"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/pty"
	"github.com/cfoust/cy/pkg/session"
	"github.com/cfoust/cy/pkg/util"

	"github.com/sasha-s/go-deadlock"
)

type PaneContext struct {
	Directory string
	Command   string
	Args      []string
}

// A virtual terminal consisting of a program that is always restarted.
type Pane struct {
	util.Lifetime
	deadlock.Mutex

	context PaneContext
	session *session.Session
	pty     *pty.Pty

	Terminal emu.Terminal
}

func (p *Pane) pollPty() error {
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

		go p.pollPty()

		started <- nil
	}()

	err := <-started
	if err != nil {
		return err
	}

	return p.pty.Wait()
}

func NewPane(ctx context.Context, context PaneContext, size Size) *Pane {
	pane := Pane{
		Lifetime: util.NewLifetime(ctx),
		context:  context,
		Terminal: emu.New(emu.WithSize(
			size.Cols,
			size.Rows,
		)),
		session: session.New(),
	}

	pane.session.Resize(size.Cols, size.Rows)

	return &pane
}
