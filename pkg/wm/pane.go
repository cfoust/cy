package wm

import (
	"context"
	"io"
	"time"

	"github.com/cfoust/cy/pkg/app"
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/session"
	"github.com/cfoust/cy/pkg/util"

	"github.com/sasha-s/go-deadlock"
)

type Pane struct {
	deadlock.RWMutex
	*metaData

	session *session.Session
	app     app.App

	changes  *util.Publisher[time.Time]
	Terminal emu.Terminal
}

var _ Node = (*Pane)(nil)

// Subscribe to state updates.
func (p *Pane) Subscribe() *util.Subscriber[time.Time] {
	return p.changes.Subscribe()
}

func (p *Pane) App() app.App {
	return p.app
}

func (p *Pane) GetSize() geom.Size {
	p.Terminal.Lock()
	cols, rows := p.Terminal.Size()
	p.Terminal.Unlock()

	return geom.Size{
		Rows:    rows,
		Columns: cols,
	}
}

func (p *Pane) notifyChange() {
	p.changes.Publish(time.Now())
}

func (p *Pane) Resize(size geom.Size) {
	p.Terminal.Resize(size.Columns, size.Rows)
	p.app.Resize(size)
	p.notifyChange()
}

func (p *Pane) pollIO(ctx context.Context) error {
	buffer := make([]byte, 4096)

	for {
		numBytes, err := p.app.Read(buffer)
		if err == io.EOF {
			return nil
		}
		if err != nil {
			// TODO(cfoust): 05/17/23
			return err
		}
		if ctx.Err() != nil {
			return ctx.Err()
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

func (p *Pane) Write(data []byte) (n int, err error) {
	p.session.Input(data)
	return p.app.Write(data)
}

func newPane(ctx context.Context, app app.App, size geom.Size) *Pane {
	pane := Pane{
		session: session.New(),
		Terminal: emu.New(emu.WithSize(
			size.Columns,
			size.Rows,
		)),
		changes: util.NewPublisher[time.Time](),
		app:     app,
	}

	pane.session.Resize(size.Columns, size.Rows)

	go pane.pollIO(ctx)

	return &pane
}
