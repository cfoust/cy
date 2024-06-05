package replay

import (
	"context"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	S "github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/replay/movement"
	"github.com/cfoust/cy/pkg/replay/player"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	"github.com/sasha-s/go-deadlock"
)

type Replayable struct {
	util.Lifetime
	deadlock.RWMutex
	*mux.UpdatePublisher

	size        geom.Size
	cmd, stream mux.Stream
	terminal    *S.Terminal
	replay      *taro.Program
	player      *player.Player

	binds *bind.BindScope
}

var _ mux.Screen = (*Replayable)(nil)

func (r *Replayable) Stream() mux.Stream {
	return r.stream
}

// TODO(cfoust): 04/07/24 This is a dirty hack specifically for (cmd/path).
func (r *Replayable) Cmd() mux.Stream {
	return r.cmd
}

func (r *Replayable) Screen() mux.Screen {
	return r.terminal
}

func (r *Replayable) Commands() []player.Command {
	return r.player.Commands()
}

func (r *Replayable) Preview(
	location geom.Vec2,
	highlights []movement.Highlight,
) *tty.State {
	r.RLock()
	size := r.size
	r.RUnlock()
	return r.player.Preview(size, location, highlights)
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
	r.Lock()
	r.size = size
	r.Unlock()

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

func (r *Replayable) EnterReplay(options ...Option) {
	if r.isReplayMode() {
		return
	}

	r.Lock()
	defer r.Unlock()

	r.player.Acquire()
	replay := New(r.Ctx(), r.player, r.binds, options...)

	replay.Resize(r.size)
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
	cmd, stream mux.Stream,
	binds *bind.BindScope,
) *Replayable {
	lifetime := util.NewLifetime(ctx)
	r := &Replayable{
		Lifetime:        lifetime,
		UpdatePublisher: mux.NewPublisher(),
		binds:           binds,
		cmd:             cmd,
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
