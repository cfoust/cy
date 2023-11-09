package replayable

import (
	"context"

	"github.com/cfoust/cy/pkg/bind"
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

	screen   mux.Screen
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
	return r.screen
}

func (r *Replayable) EnterReplay() {
	r.Lock()
	defer r.Unlock()

	if r.NumLayers() > 1 {
		return
	}

	events := make(chan mux.Msg)
	replay := replay.New(
		r.Ctx(),
		r.recorder.Events(),
		r.binds,
	)

	r.NewLayer(
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
		r.Unlock()
	}()
}

func New(
	ctx context.Context,
	screen mux.Screen,
	stream mux.Stream,
	recorder *sessions.Recorder,
	binds *bind.BindScope,
) *Replayable {
	lifetime := util.NewLifetime(ctx)
	layers := S.NewLayers()
	layers.NewLayer(
		lifetime.Ctx(),
		screen,
		S.PositionTop,
		S.WithInteractive,
		S.WithOpaque,
	)

	return &Replayable{
		Lifetime: lifetime,
		binds:    binds,
		screen:   screen,
		stream:   stream,
		recorder: recorder,
		Layers:   layers,
	}
}
