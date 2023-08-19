package tree

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/mux/screen"
	copyMode "github.com/cfoust/cy/pkg/mux/screen/copy"
	"github.com/cfoust/cy/pkg/mux/stream"
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

func (p *Pane) CopyMode(info screen.RenderContext) {
	if p.layers.NumLayers() > 1 {
		return
	}

	copyMode := copyMode.New(
		p.Ctx(),
		info,
		p.terminal.History(),
		geom.DEFAULT_SIZE,
	)

	p.layers.NewLayer(
		copyMode.Ctx(),
		copyMode,
		true,
		true,
	)
}

func newPane(ctx context.Context, subStream stream.Stream, size geom.Vec2) *Pane {
	lifetime := util.NewLifetime(ctx)
	recorder := sessions.NewRecorder(subStream)
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
