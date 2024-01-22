package screen

import (
	"context"
	"io"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/taro"
)

type Terminal struct {
	*mux.UpdatePublisher
	terminal emu.Terminal
	stream   Stream
}

var _ Screen = (*Terminal)(nil)

func (t *Terminal) State() *tty.State {
	return tty.Capture(t.terminal)
}

func (t *Terminal) Resize(size Size) error {
	t.terminal.Resize(size.C, size.R)

	err := t.stream.Resize(size)
	if err != nil {
		return err
	}

	t.Notify()
	return nil
}

func (t *Terminal) Send(msg mux.Msg) {
	input := make([]byte, 0)
	mode := t.terminal.Mode()

	switch msg := msg.(type) {
	case taro.KeyMsg:
		// TODO(cfoust): 01/22/24 error handling
		data, _ := taro.KeysToBytes(msg)
		input = data
	case taro.MouseMsg:
		switch mode & emu.ModeMouseMask {
		case emu.ModeMouseX10:
			if msg.Type != taro.MousePress {
				return
			}

			input = taro.MouseEvent(msg).X10Bytes()
		case emu.ModeMouseButton:
			// TODO(cfoust): 08/08/23 we should still report drag
			if msg.Type == taro.MouseMotion {
				return
			}
			input = msg.Bytes()
		case emu.ModeMouseMotion:
			input = msg.Bytes()
		case 0:
			return
		}
	}

	if len(input) == 0 {
		return
	}

	// TODO(cfoust): 11/08/23 error handling
	t.stream.Write(input)
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
		t.Notify()
	}
}

func NewTerminal(ctx context.Context, stream Stream, size Size) *Terminal {
	terminal := &Terminal{
		UpdatePublisher: mux.NewPublisher(),
		terminal:        emu.New(emu.WithWriter(stream), emu.WithSize(size)),
		stream:          stream,
	}

	// TODO(cfoust): 07/14/23 error handling
	go terminal.poll(ctx)

	return terminal
}
