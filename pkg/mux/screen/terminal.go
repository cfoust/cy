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

func (t *Terminal) Kill() {
	t.stream.Kill()
}

func (t *Terminal) State() *tty.State {
	return tty.Capture(t.terminal)
}

func (t *Terminal) Resize(size Size) error {
	t.terminal.Resize(size)

	err := t.stream.Resize(size)
	if err != nil {
		return err
	}

	t.Notify()
	return nil
}

func (t *Terminal) IsAltMode() bool {
	return t.terminal.IsAltMode()
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

func (t *Terminal) Write(p []byte) (n int, err error) {
	n, err = t.terminal.Write(p)
	if err != nil {
		return 0, err
	}

	// Let any clients know that this pane changed
	t.Notify()

	return n, err
}

func NewTerminal(
	ctx context.Context,
	stream Stream,
	size Size,
	options ...emu.TerminalOption,
) *Terminal {
	options = append(options, emu.WithWriter(stream), emu.WithSize(size))

	terminal := &Terminal{
		UpdatePublisher: mux.NewPublisher(),
		terminal:        emu.New(options...),
		stream:          stream,
	}

	go io.Copy(terminal, stream)

	return terminal
}
