package preview

import (
	"context"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/params"
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

func ParamsToReplayOptions(
	altScreen *bool,
	focus *geom.Vec2,
	highlights *[]search.Selection,
) (options []replay.Option) {
	if altScreen != nil && !*altScreen {
		options = append(
			options,
			replay.WithFlow,
		)
	}

	if focus != nil {
		options = append(
			options,
			replay.WithLocation(*focus),
		)
	}

	if highlights == nil {
		return
	}

	fixedHighlights := make([]movement.Highlight, 0, len(*highlights))
	render := taro.NewRenderer()
	fgColor := render.LipglossToEmu(lipgloss.Color("1"))
	bgColor := render.LipglossToEmu(lipgloss.Color("14"))

	for _, highlight := range *highlights {
		fixedHighlights = append(
			fixedHighlights,
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
		replay.WithHighlights(fixedHighlights),
	)
	return
}

func NewReplay(
	ctx context.Context,
	params *params.Parameters,
	args ReplayType,
) mux.Screen {
	options := ParamsToReplayOptions(
		args.AltScreen,
		args.Focus,
		args.Highlights,
	)

	options = append(
		options,
		replay.WithParams(params),
	)

	return loader.New(
		ctx,
		params,
		bind.NewBindScope(nil),
		bind.NewBindScope(nil),
		args.Path,
		options...,
	)
}
