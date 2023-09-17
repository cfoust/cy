package tree

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/mux/stream"
	"github.com/cfoust/cy/pkg/sessions"
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

func newPane(ctx context.Context, subStream stream.Stream, sessionFile string, size geom.Vec2) *Pane {
	lifetime := util.NewLifetime(ctx)
	recorder, _ := sessions.NewRecorder(sessionFile, subStream)
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
	pane := Pane{
		Lifetime: lifetime,
		layers:   layers,
		recorder: recorder,
		screen:   layers,
		stream:   subStream,
		terminal: terminal,
	}

	return &pane
}
