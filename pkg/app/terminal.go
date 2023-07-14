package app

import (
	"context"
	"io"
	"time"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/util"
)

type Terminal struct {
	terminal emu.Terminal
	app      App
	changes  *util.Publisher[time.Time]
}

var _ App = (*Terminal)(nil)

func (t *Terminal) Emu() emu.Terminal {
	return t.terminal
}

func (t *Terminal) notifyChange() {
	t.changes.Publish(time.Now())
}

func (t *Terminal) Subscribe() *util.Subscriber[time.Time] {
	return t.changes.Subscribe()
}

func (t *Terminal) Resize(size Size) error {
	t.terminal.Resize(size.Columns, size.Rows)
	t.app.Resize(size)
	t.notifyChange()
	return nil
}

func (t *Terminal) Write(data []byte) (n int, err error) {
	return t.app.Write(data)
}

// Terminals swallow all of their output to influence the display state, so
// this does nothing.
func (t *Terminal) Read(p []byte) (n int, err error) {
	return 0, nil
}

func (t *Terminal) poll(ctx context.Context) error {
	buffer := make([]byte, 4096)

	for {
		numBytes, err := t.app.Read(buffer)
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

		_, err = t.terminal.Write(copied)
		if err != nil {
			return err
		}

		// Let any clients know that this pane changed
		t.notifyChange()
	}
}

func NewTerminal(ctx context.Context, app App, size Size) *Terminal {
	terminal := &Terminal{
		terminal: emu.New(emu.WithSize(
			size.Columns,
			size.Rows,
		)),
		changes: util.NewPublisher[time.Time](),
		app:     app,
	}

	// TODO(cfoust): 07/14/23 error handling
	go terminal.poll(ctx)

	return terminal
}
