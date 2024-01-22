package replayable

import (
	"context"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	S "github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/mux/screen/replay"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	"github.com/sasha-s/go-deadlock"
)

type Replayable struct {
	deadlock.RWMutex
	util.Lifetime
	*S.Layers

	terminal *S.Terminal
	stream   mux.Stream
	recorder *sessions.Recorder
	replay   *taro.Program
	binds    *bind.BindScope
}

var _ mux.Screen = (*Replayable)(nil)

func (r *Replayable) Stream() mux.Stream {
	return r.stream
}

func (r *Replayable) Screen() mux.Screen {
	return r.terminal
}

func (r *Replayable) Send(msg mux.Msg) {
	// We want to automatically trigger replay mode when the user scrolls
	// up with the mouse
	if mouse, ok := msg.(taro.MouseMsg); ok {
		isMouseUp := mouse.Type == taro.MousePress && mouse.Button == taro.MouseWheelUp
		if isMouseUp && !r.terminal.IsAltMode() && r.Layers.NumLayers() == 1 {
			r.EnterReplay()
			return
		}
	}

	r.Layers.Send(msg)
}

func (r *Replayable) EnterReplay() {
	r.Lock()
	defer r.Unlock()

	if r.Layers.NumLayers() > 1 {
		return
	}

	events := make(chan mux.Msg)
	replay := replay.New(
		r.Ctx(),
		r.recorder.Events(),
		r.binds,
	)

	r.Layers.NewLayer(
		replay.Ctx(),
		replay,
		S.PositionTop,
		S.WithInteractive,
		S.WithOpaque,
	)
	r.replay = replay

	go func() {
		for {
			select {
			case event := <-events:
				r.Layers.Publish(event)
			case <-replay.Ctx().Done():
				return
			}
		}
	}()

	go func() {
		<-replay.Ctx().Done()
		r.Lock()
		r.replay = nil
		r.Unlock()
	}()
}

func New(
	ctx context.Context,
	stream mux.Stream,
	recorder *sessions.Recorder,
	binds *bind.BindScope,
) *Replayable {
	lifetime := util.NewLifetime(ctx)
	terminal := S.NewTerminal(lifetime.Ctx(), recorder, geom.DEFAULT_SIZE)
	layers := S.NewLayers()
	layers.NewLayer(
		lifetime.Ctx(),
		terminal,
		S.PositionTop,
		S.WithInteractive,
		S.WithOpaque,
	)

	return &Replayable{
		Lifetime: lifetime,
		binds:    binds,
		terminal: terminal,
		stream:   stream,
		recorder: recorder,
		Layers:   layers,
	}
}
