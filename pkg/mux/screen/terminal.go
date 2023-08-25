package screen

import (
	"context"
	"io"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/latte"
	"github.com/cfoust/cy/pkg/mux"
)

type Terminal struct {
	terminal emu.Terminal
	stream   Stream
	changes  *mux.UpdatePublisher
}

var _ Screen = (*Terminal)(nil)

func (t *Terminal) State() *tty.State {
	return tty.Capture(t.terminal)
}

func (t *Terminal) notifyChange() {
	t.changes.Publish(t.State())
}

func (t *Terminal) Updates() *Updater {
	return t.changes.Subscribe()
}

func (t *Terminal) Resize(size Size) error {
	t.terminal.Resize(size.C, size.R)

	err := t.stream.Resize(size)
	if err != nil {
		return err
	}

	t.notifyChange()
	return nil
}

func (t *Terminal) Write(data []byte) (n int, err error) {
	mode := t.terminal.Mode()

	input := make([]byte, 0)
	var msg latte.Msg
	for i, w := 0, 0; i < len(data); i += w {
		w, msg = latte.DetectOneMsg(data[i:])
		if msg == nil {
			continue
		}

		if _, ok := msg.(latte.KeyMsg); ok {
			input = append(
				input,
				data[i:i+w]...,
			)
			continue
		}

		mouse, ok := msg.(latte.MouseMsg)
		if !ok {
			continue
		}

		switch mode & emu.ModeMouseMask {
		case emu.ModeMouseX10:
			if mouse.Type != latte.MouseLeft {
				continue
			}

			input = append(
				input,
				latte.MouseEvent(mouse).X10Bytes()...,
			)
			continue
		case emu.ModeMouseButton:
			// TODO(cfoust): 08/08/23 we should still report drag
			if mouse.Type == latte.MouseMotion {
				continue
			}
		case 0:
			continue
		}

		input = append(
			input,
			data[i:i+w]...,
		)
	}

	return t.stream.Write(input)
}

func (t *Terminal) poll(ctx context.Context) error {
	// TODO(cfoust): 07/17/23 replace with io.Copy
	buffer := make([]byte, 4096)

	for {
		numBytes, err := t.stream.Read(buffer)
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

func NewTerminal(ctx context.Context, stream Stream, size Size) *Terminal {
	terminal := &Terminal{
		terminal: emu.New(emu.WithWriter(stream), emu.WithSize(size)),
		changes:  mux.NewPublisher(),
		stream:   stream,
	}

	// TODO(cfoust): 07/14/23 error handling
	go terminal.poll(ctx)

	return terminal
}
