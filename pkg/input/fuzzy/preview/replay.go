package preview

import (
	"context"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/replay"
	"github.com/cfoust/cy/pkg/replay/loader"
	"github.com/cfoust/cy/pkg/replay/movement"
	"github.com/cfoust/cy/pkg/sessions/search"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/charmbracelet/lipgloss"
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

	if args.Highlights != nil {
		highlights := make([]movement.Highlight, 0, len(*args.Highlights))
		render := taro.NewRenderer()
		fgColor := render.ConvertLipgloss(lipgloss.Color("1"))
		bgColor := render.ConvertLipgloss(lipgloss.Color("14"))

		for _, highlight := range *args.Highlights {
			highlights = append(
				highlights,
				movement.Highlight{
					From: highlight.From,
					To:   highlight.To,
					FG:   fgColor,
					BG:   bgColor,
				},
			)
		}

		options = append(
			options,
			replay.WithHighlights(highlights),
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
