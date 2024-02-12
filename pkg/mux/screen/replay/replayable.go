package replay

import (
	"context"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	S "github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	"github.com/sasha-s/go-deadlock"
)

type Replayable struct {
	util.Lifetime
	deadlock.RWMutex
	*mux.UpdatePublisher

	stream mux.Stream

	showPlayer bool

	terminal *S.Terminal
	replay   *taro.Program

	binds *bind.BindScope
}

var _ mux.Screen = (*Replayable)(nil)
var _ sessions.EventHandler = (*Replayable)(nil)

func (r *Replayable) Stream() mux.Stream {
	return r.stream
}

func (r *Replayable) Screen() mux.Screen {
	return r.terminal
}

func (r *Replayable) isPlayerMode() bool {
	r.RLock()
	showPlayer := r.showPlayer
	r.RUnlock()
	return showPlayer
}

func (r *Replayable) State() *tty.State {
	var currentScreen mux.Screen = r.terminal
	if r.isPlayerMode() {
		currentScreen = r.replay
	}

	return currentScreen.State()
}

func (r *Replayable) Process(event sessions.Event) error {
	return nil
}

func (r *Replayable) Resize(size geom.Size) error {
	err := r.terminal.Resize(size)
	if err != nil {
		return err
	}

	return r.replay.Resize(size)
}

func (r *Replayable) poll(ctx context.Context) {
	terminalEvents := r.terminal.Subscribe(ctx)
	replayEvents := r.replay.Subscribe(ctx)

	for {
		select {
		case <-ctx.Done():
			return
		case event := <-terminalEvents.Recv():
			if r.isPlayerMode() {
				continue
			}
			r.Publish(event)
		case event := <-replayEvents.Recv():
			if !r.isPlayerMode() {
				continue
			}
			r.Publish(event)
		}
	}
}

func (r *Replayable) Send(msg mux.Msg) {
	if r.isPlayerMode() {
		r.replay.Send(msg)
		return
	}

	// We want to automatically trigger replay mode when the user scrolls
	// up with the mouse
	if mouse, ok := msg.(taro.MouseMsg); ok {
		isMouseUp := mouse.Type == taro.MousePress && mouse.Button == taro.MouseWheelUp
		if isMouseUp && !r.terminal.IsAltMode() {
			r.EnterReplay()
			return
		}
	}

	r.terminal.Send(msg)
}

func (r *Replayable) EnterReplay() {
	r.Lock()
	defer r.Unlock()
	r.showPlayer = true
}

func NewReplayable(
	ctx context.Context,
	stream mux.Stream,
	binds *bind.BindScope,
) *Replayable {
	lifetime := util.NewLifetime(ctx)
	r := &Replayable{
		Lifetime:        lifetime,
		UpdatePublisher: mux.NewPublisher(),
		binds:           binds,
		stream:          stream,
		replay: New(
			lifetime.Ctx(),
			[]sessions.Event{},
			binds,
		),
	}
	r.terminal = S.NewTerminal(
		lifetime.Ctx(),
		stream,
		geom.DEFAULT_SIZE,
	)

	go r.poll(ctx)

	return r
}
