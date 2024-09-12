package preview

import (
	"context"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/replay"
	"github.com/cfoust/cy/pkg/replay/loader"
	"github.com/cfoust/cy/pkg/sessions/search"
)

type ReplayType struct {
	Path       string
	AltScreen  *bool
	Focus      *geom.Vec2
	Highlights *[]search.Selection
}

func NewReplay(
	ctx context.Context,
	args ReplayType,
) mux.Screen {
	var options []replay.Option

	if args.AltScreen != nil && !*args.AltScreen {
		options = append(
			options,
			replay.WithFlow,
		)
	}

	if args.Focus != nil {
		options = append(
			options,
			replay.WithLocation(*args.Focus),
		)
	}

	return loader.New(
		ctx,
		args.Path,
		bind.NewBindScope(nil),
		bind.NewBindScope(nil),
		options...,
	)
}
