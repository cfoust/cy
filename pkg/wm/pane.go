package wm

import (
	"context"

	"github.com/cfoust/cy/pkg/app"
	"github.com/cfoust/cy/pkg/geom"

	"github.com/sasha-s/go-deadlock"
)

type Pane struct {
	deadlock.RWMutex
	*metaData

	recorder *app.Recorder
	terminal *app.Terminal
	app      app.App
}

var _ Node = (*Pane)(nil)

func (p *Pane) App() app.App {
	return p.app
}

func (p *Pane) Terminal() *app.Terminal {
	return p.terminal
}

func (p *Pane) Resize(size geom.Size) {
	p.terminal.Resize(size)
}

func (p *Pane) Write(data []byte) (n int, err error) {
	return p.app.Write(data)
}

func newPane(ctx context.Context, subApp app.App, size geom.Size) *Pane {
	recorder := app.NewRecorder(subApp)
	terminal := app.NewTerminal(
		ctx,
		recorder,
		size,
	)
	pane := Pane{
		terminal: terminal,
		recorder: recorder,
		app:      subApp,
	}

	return &pane
}
