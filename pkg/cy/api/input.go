package api

import (
	"context"
	"fmt"

	"github.com/cfoust/cy/pkg/fuzzy"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/util"
)

type InputModule struct {
	Lifetime util.Lifetime
	Tree     *tree.Tree
	Server   *server.Server
}

type FuzzyParams struct {
	Prompt   string
	Full     bool
	Reverse  bool
	Animated *bool
	Headers  *[]string
}

func (i *InputModule) Find(
	ctx context.Context,
	user interface{},
	choices *janet.Value,
	named *janet.Named[FuzzyParams],
) (interface{}, error) {
	defer choices.Free()

	params := named.Values()

	client, ok := user.(Client)
	if !ok {
		return nil, fmt.Errorf("missing client context")
	}

	options, err := fuzzy.UnmarshalOptions(choices)
	if err != nil {
		return nil, err
	}

	shouldAnimate := client.Params().Animate()
	outerLayers := client.OuterLayers()
	state := outerLayers.State()
	cursor := state.Cursor
	result := make(chan interface{})

	settings := []fuzzy.Setting{
		fuzzy.WithNodes(
			i.Tree,
			i.Server.AddClient(ctx, geom.Vec2{}),
		),
		fuzzy.WithResult(result),
		fuzzy.WithPrompt(params.Prompt),
		fuzzy.WithInline(
			geom.Vec2{R: cursor.R, C: cursor.C},
			state.Image.Size(),
		),
	}

	if (params.Animated == nil || (*params.Animated) == true) && shouldAnimate {
		settings = append(settings, fuzzy.WithAnimation(state.Image))
	}

	if params.Headers != nil {
		settings = append(
			settings,
			fuzzy.WithHeaders(*params.Headers...),
		)
	}

	fuzzy := fuzzy.New(
		ctx,
		options,
		settings...,
	)

	outerLayers.NewLayer(
		fuzzy.Ctx(),
		fuzzy,
		screen.PositionTop,
		screen.WithInteractive,
		screen.WithOpaque,
	)

	select {
	case match := <-result:
		return match, nil
	case <-ctx.Done():
		return nil, ctx.Err()
	}
}
