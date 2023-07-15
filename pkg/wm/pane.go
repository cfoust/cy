package wm

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/ui"
	"github.com/cfoust/cy/pkg/ui/io"
	"github.com/cfoust/cy/pkg/ui/screen"

	"github.com/sasha-s/go-deadlock"
)

type Pane struct {
	deadlock.RWMutex
	*metaData

	recorder *io.Recorder
	terminal *screen.Terminal
	io       ui.IO
}

var _ Node = (*Pane)(nil)

func (p *Pane) App() ui.IO {
	return p.io
}

func (p *Pane) Terminal() *screen.Terminal {
	return p.terminal
}

func (p *Pane) Resize(size geom.Size) {
	p.terminal.Resize(size)
}

func (p *Pane) Write(data []byte) (n int, err error) {
	return p.io.Write(data)
}

func newPane(ctx context.Context, subApp ui.IO, size geom.Size) *Pane {
	recorder := io.NewRecorder(subApp)
	terminal := screen.NewTerminal(
		ctx,
		recorder,
		size,
	)
	pane := Pane{
		terminal: terminal,
		recorder: recorder,
		io:       subApp,
	}

	return &pane
}
