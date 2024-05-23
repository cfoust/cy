package preview

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	I "github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/taro"
)

type TextType struct {
	Text string
}

func NewText(ctx context.Context, args TextType) mux.Screen {
	image := I.New(geom.DEFAULT_SIZE)
	render := taro.NewRenderer()
	render.RenderAt(
		image,
		0, 0,
		render.NewStyle().
			MaxWidth(geom.DEFAULT_SIZE.C).
			MaxHeight(geom.DEFAULT_SIZE.R).
			Render(args.Text),
	)

	return taro.New(ctx, &Static{image: image})
}
