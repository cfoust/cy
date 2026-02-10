package screen

import (
	"context"
	"fmt"
	"io"
	"os/exec"
	"time"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/keys"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/params"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/charmbracelet/lipgloss"
	"github.com/sasha-s/go-deadlock"
)

type ExitEvent struct {
	Errored bool
	Code    int
}

type Terminal struct {
	deadlock.RWMutex
	*mux.UpdatePublisher
	render *taro.Renderer
	params *params.Parameters

	terminal  emu.Terminal
	stream    Stream
	size      geom.Size
	exited    bool
	exitError error

	syncTimerMu deadlock.Mutex
	syncTimer   *time.Timer
}

var _ Screen = (*Terminal)(nil)

func (t *Terminal) Kill() {
	t.stream.Kill()
}

func (t *Terminal) State() *tty.State {
	t.RLock()
	state := tty.Capture(t.terminal)
	var (
		exited    = t.exited
		exitError = t.exitError
	)
	t.RUnlock()

	size := state.Image.Size()

	if !exited {
		return state
	}

	p := t.params
	message := p.TerminalTextExited()
	offsetStyle := t.render.NewStyle().
		Foreground(p.ColorInfo()).
		Background(lipgloss.Color("8"))

	if realError, ok := exitError.(*exec.ExitError); ok {
		offsetStyle = t.render.NewStyle().
			Foreground(p.ColorError()).
			Background(lipgloss.Color("8"))

		message = fmt.Sprintf(
			message+" (%d)",
			realError.ExitCode(),
		)
	}

	t.render.RenderAt(
		state.Image,
		0,
		0,
		t.render.PlaceHorizontal(
			size.C,
			lipgloss.Right,
			offsetStyle.Render(fmt.Sprintf(
				"[%s]",
				message,
			)),
		),
	)
	return state
}

func (t *Terminal) Resize(size Size) error {
	t.Lock()
	if size == t.size {
		t.Unlock()
		return nil
	}

	t.size = size
	t.terminal.Resize(size)
	t.Unlock()

	if t.stream != nil {
		err := t.stream.Resize(size)
		if err != nil {
			return err
		}
	}

	t.Notify()
	return nil
}

func (t *Terminal) IsAltMode() bool {
	return t.terminal.IsAltMode()
}

func (t *Terminal) Title() string {
	return t.terminal.Title()
}

func (t *Terminal) Send(msg mux.Msg) {
	if t.stream == nil {
		return
	}

	var (
		input = make([]byte, 0)
		mode  = t.terminal.Mode()
		ok    bool
	)

	switch msg := msg.(type) {
	case taro.KittyKeyMsg:
		input, ok = keys.Key(msg).Bytes(mode, t.terminal.KeyState())
	case taro.MouseMsg:
		input, ok = keys.Mouse(msg).Bytes(mode)
	}

	if !ok || len(input) == 0 {
		return
	}

	// TODO(cfoust): 11/08/23 error handling
	_, _ = t.stream.Write(input)
}

func (t *Terminal) Write(p []byte) (n int, err error) {
	t.Lock()
	n, syncing, err := t.terminal.WriteSync(p)
	t.Unlock()
	if err != nil {
		return 0, err
	}

	if syncing {
		t.ensureSyncTimeout()
	} else {
		t.cancelSyncTimeout()
		t.Notify()
	}

	return n, nil
}

func (t *Terminal) ensureSyncTimeout() {
	t.syncTimerMu.Lock()
	defer t.syncTimerMu.Unlock()
	if t.syncTimer != nil {
		return
	}
	t.syncTimer = time.AfterFunc(
		200*time.Millisecond,
		func() {
			t.syncTimerMu.Lock()
			t.syncTimer = nil
			t.syncTimerMu.Unlock()
			t.Notify()
		},
	)
}

func (t *Terminal) cancelSyncTimeout() {
	t.syncTimerMu.Lock()
	defer t.syncTimerMu.Unlock()
	if t.syncTimer != nil {
		t.syncTimer.Stop()
		t.syncTimer = nil
	}
}

func NewTerminal(
	ctx context.Context,
	stream Stream,
	size Size,
	params *params.Parameters,
	options ...emu.TerminalOption,
) *Terminal {
	options = append(options,
		emu.WithWriter(stream),
		emu.WithSize(size),
	)

	t := &Terminal{
		UpdatePublisher: mux.NewPublisher(),
		terminal:        emu.New(options...),
		stream:          stream,
		render:          taro.NewRenderer(),
		params:          params,
	}

	go func() {
		_, err := io.Copy(t, stream)
		t.Lock()
		t.exited = true
		t.exitError = err
		t.Unlock()

		if exitError, ok := err.(*exec.ExitError); ok {
			t.Publish(ExitEvent{
				Errored: true,
				Code:    exitError.ExitCode(),
			})
		} else {
			t.Publish(ExitEvent{})
		}
	}()

	return t
}

func NewStaticTerminal(
	ctx context.Context,
	term emu.Terminal,
) *Terminal {
	return &Terminal{
		UpdatePublisher: mux.NewPublisher(),
		terminal:        term,
		render:          taro.NewRenderer(),
		params:          params.New(),
	}
}
