package tree

import (
	"context"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/mux/screen/replay"
	"github.com/cfoust/cy/pkg/mux/stream"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"
)

type Pane struct {
	util.Lifetime
	*metaData

	terminal *screen.Terminal
	recorder *sessions.Recorder
	screen   mux.Screen
	layers   *screen.Layers
	stream   stream.Stream

	replay       *taro.Program
	replayBinds  *bind.BindScope
	replayEvents chan<- bind.BindEvent
}

var _ Node = (*Pane)(nil)

func (p *Pane) App() stream.Stream {
	return p.stream
}

func (p *Pane) Screen() mux.Screen {
	return p.screen
}

func (p *Pane) Recorder() *sessions.Recorder {
	return p.recorder
}

func (p *Pane) Resize(size geom.Vec2) {
	p.screen.Resize(size)
}

func (p *Pane) Write(data []byte) (n int, err error) {
	return p.stream.Write(data)
}

func (p *Pane) ReplayMode() *taro.Program {
	p.RLock()
	r := p.replay
	p.RUnlock()
	return r
}

func (p *Pane) EnterReplay() {
	p.Lock()
	if p.layers.NumLayers() > 1 {
		return
	}

	r := replay.New(
		p.Ctx(),
		p.Recorder(),
		p.replayBinds,
		p.replayEvents,
	)

	p.layers.NewLayer(
		r.Ctx(),
		r,
		true,
		true,
	)
	p.replay = r
	p.Unlock()

	go func() {
		<-r.Ctx().Done()
		p.Lock()
		p.replay = nil
		p.Unlock()
	}()
}

func newPane(
	ctx context.Context,
	id NodeID,
	subStream stream.Stream,
	sessionFile string,
	size geom.Vec2,
	replayBinds *bind.BindScope,
	replayEvents chan<- ReplayEvent,
) *Pane {
	lifetime := util.NewLifetime(ctx)
	// TODO(cfoust): 09/19/23 error handling
	recorder, _ := sessions.NewRecorder(ctx, sessionFile, subStream)
	terminal := screen.NewTerminal(
		lifetime.Ctx(),
		recorder,
		size,
	)
	layers := screen.NewLayers()
	layers.NewLayer(
		lifetime.Ctx(),
		terminal,
		true,
		true,
	)

	actions := make(chan bind.BindEvent)
	go func() {
		for {
			select {
			case <-lifetime.Ctx().Done():
				return
			case event := <-actions:
				replayEvents <- ReplayEvent{
					Id:    id,
					Event: event,
				}
			}
		}
	}()

	pane := Pane{
		Lifetime:     lifetime,
		layers:       layers,
		recorder:     recorder,
		screen:       layers,
		stream:       subStream,
		terminal:     terminal,
		replayBinds:  replayBinds,
		replayEvents: actions,
	}

	return &pane
}
