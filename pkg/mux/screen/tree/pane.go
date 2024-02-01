package tree

import (
	"context"

	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/util"
)

type Pane struct {
	util.Lifetime
	*metaData
	screen mux.Screen
}

var _ Node = (*Pane)(nil)

func (p *Pane) Screen() mux.Screen {
	return p.screen
}

func newPane(
	ctx context.Context,
	s mux.Screen,
) *Pane {
	return &Pane{
		Lifetime: util.NewLifetime(ctx),
		screen:   s,
	}
}
