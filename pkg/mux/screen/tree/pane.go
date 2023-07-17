package tree

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/mux/stream"

	"github.com/sasha-s/go-deadlock"
)

type Pane struct {
	deadlock.RWMutex
	*metaData

	recorder *stream.Recorder
	screen   mux.Screen
	stream   stream.Stream
}

var _ Node = (*Pane)(nil)

func (p *Pane) App() stream.Stream {
	return p.stream
}

func (p *Pane) Screen() mux.Screen {
	return p.screen
}

func (p *Pane) Resize(size geom.Vec2) {
	p.screen.Resize(size)
}

func (p *Pane) Write(data []byte) (n int, err error) {
	return p.stream.Write(data)
}

func newPane(ctx context.Context, subStream stream.Stream, size geom.Vec2) *Pane {
	recorder := stream.NewRecorder(subStream)
	terminal := screen.NewTerminal(
		ctx,
		recorder,
		size,
	)
	pane := Pane{
		screen:   terminal,
		recorder: recorder,
		stream:   subStream,
	}

	return &pane
}
