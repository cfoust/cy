package replay

import (
	"context"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	S "github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/mux/screen/replay/player"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	"github.com/sasha-s/go-deadlock"
)

type Replayable struct {
	util.Lifetime
	deadlock.RWMutex
	*mux.UpdatePublisher

	stream   mux.Stream
	terminal *S.Terminal
	replay   *taro.Program
	player   *player.Player

	binds *bind.BindScope
}

var _ mux.Screen = (*Replayable)(nil)

func (r *Replayable) Stream() mux.Stream {
	return r.stream
}

func (r *Replayable) Screen() mux.Screen {
	return r.terminal
}

func (r *Replayable) isReplayMode() bool {
	r.RLock()
	showReplay := r.replay != nil
	r.RUnlock()
	return showReplay
}

func (r *Replayable) State() *tty.State {
	var currentScreen mux.Screen = r.terminal
	if r.isReplayMode() {
		currentScreen = r.replay
	}

	return currentScreen.State()
}

func (r *Replayable) Resize(size geom.Size) error {
	err := r.terminal.Resize(size)
	if err != nil {
		return err
	}

	if !r.isReplayMode() {
		return nil
	}

	return r.replay.Resize(size)
}

func (r *Replayable) poll(ctx context.Context) {
	terminalEvents := r.terminal.Subscribe(ctx)

	for {
		select {
		case <-ctx.Done():
			return
		case event := <-terminalEvents.Recv():
			if r.isReplayMode() {
				continue
			}
			r.Publish(event)
		}
	}
}

func (r *Replayable) Send(msg mux.Msg) {
	if r.isReplayMode() {
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
	if r.isReplayMode() {
		return
	}

	r.Lock()
	defer r.Unlock()

	r.player.Acquire()
	replay := New(
		r.Ctx(),
		[]sessions.Event{},
		r.binds,
	)

	r.replay = replay

	go func() {
		events := replay.Subscribe(r.Ctx())
		for {
			select {
			case event := <-events.Recv():
				r.Publish(event)
			case <-replay.Ctx().Done():
				return
			}
		}
	}()

	go func() {
		<-replay.Ctx().Done()
		r.Lock()
		r.replay = nil
		r.player.Release()
		r.Unlock()
	}()
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
		player:          player.New(),
	}
	r.terminal = S.NewTerminal(
		lifetime.Ctx(),
		sessions.NewEventStream(stream, r.player),
		geom.DEFAULT_SIZE,
		emu.WithoutHistory,
	)

	go r.poll(ctx)

	return r
}
