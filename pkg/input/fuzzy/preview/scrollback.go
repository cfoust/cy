package preview

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/replay/movement"
	"github.com/cfoust/cy/pkg/replay/replayable"
	"github.com/cfoust/cy/pkg/sessions/search"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/charmbracelet/lipgloss"
)

type ScrollbackType struct {
	Id         tree.NodeID
	Focus      geom.Vec2
	Highlights []search.Selection
}

func NewScrollback(
	ctx context.Context,
	tree *tree.Tree,
	args ScrollbackType,
) mux.Screen {
	pane, ok := tree.PaneById(args.Id)
	if !ok {
		return nil
	}

	r, ok := pane.Screen().(*replayable.Replayable)
	if !ok {
		return nil
	}

	render := taro.NewRenderer()
	fgColor := render.ConvertLipgloss(lipgloss.Color("1"))
	bgColor := render.ConvertLipgloss(lipgloss.Color("14"))

	var highlights []movement.Highlight
	for _, highlight := range args.Highlights {
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

	preview := r.Preview(args.Focus, highlights)
	if preview != nil {
		return NewStatic(ctx, preview.Image)
	}

	warning := image.New(geom.DEFAULT_SIZE)
	render.RenderAt(
		warning,
		0, 0,
		lipgloss.Place(
			geom.DEFAULT_SIZE.C,
			geom.DEFAULT_SIZE.R,
			lipgloss.Center, lipgloss.Center,
			"cannot preview when pane is in replay mode",
		),
	)
	return NewStatic(ctx, warning)
}
