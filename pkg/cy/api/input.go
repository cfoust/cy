package api

import (
	"context"
	"math/rand"

	"github.com/cfoust/cy/pkg/anim"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/input/fuzzy"
	"github.com/cfoust/cy/pkg/input/text"
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
	Prompt        string
	Full          bool
	Reverse       bool
	CaseSensitive bool
	Animated      *bool
	Headers       *[]string
	Width         *int
	Height        *int
}

func (i *InputModule) Find(
	ctx context.Context,
	context interface{},
	choices *janet.Value,
	named *janet.Named[FuzzyParams],
) (interface{}, error) {
	defer choices.Free()

	params := named.Values()

	client, err := getClient(context)
	if err != nil {
		return nil, err
	}

	options, err := fuzzy.UnmarshalOptions(choices)
	if err != nil {
		return nil, err
	}

	outerLayers := client.OuterLayers()
	state := outerLayers.State()
	initial := state.Image
	result := make(chan interface{})
	settings := []fuzzy.Setting{
		fuzzy.WithNodes(i.Tree, i.Server),
		fuzzy.WithResult(result),
		fuzzy.WithPrompt(params.Prompt),
		fuzzy.WithInitial(initial),
		fuzzy.WithCaseSensitive(params.CaseSensitive),
		fuzzy.WithParams(client.Params()),
	}

	if !params.Full {
		cursor := state.Cursor
		settings = append(
			settings,
			fuzzy.WithInline(
				geom.Vec2{R: cursor.R, C: cursor.C},
				state.Image.Size(),
			),
		)
	}

	if params.Width != nil || params.Height != nil {
		size := geom.Size{}

		if params.Width != nil {
			size.C = *params.Width
		}

		if params.Height != nil {
			size.R = *params.Height
		}

		settings = append(
			settings,
			fuzzy.WithSize(size),
		)
	}

	if params.Reverse {
		settings = append(settings, fuzzy.WithReverse)
	}

	if (params.Animated == nil || (*params.Animated) == true) && client.Params().Animate() {
		var animations []anim.Creator
		for _, a := range client.Params().Animations() {
			if creator, ok := anim.Animations[a]; ok {
				animations = append(
					animations,
					creator,
				)
			}
		}

		// Add all of the defaults if the setting was empty
		if len(animations) == 0 {
			for _, creator := range anim.Animations {
				animations = append(
					animations,
					creator,
				)
			}
		}

		creator := animations[rand.Int()%len(animations)]
		settings = append(
			settings,
			fuzzy.WithAnimation(initial, creator),
		)
	}

	if params.Headers != nil {
		settings = append(
			settings,
			fuzzy.WithHeaders(*params.Headers...),
		)
	}

	if client.Params().SkipInput() {
		if len(options) == 0 {
			return nil, nil
		}

		return options[0].Result, nil
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

type TextParams struct {
	Placeholder *string
	Preset      *string
	Full        bool
	Reverse     bool
	Animated    *bool
	Single      bool
}

func (i *InputModule) Text(
	ctx context.Context,
	context interface{},
	prompt string,
	named *janet.Named[TextParams],
) (interface{}, error) {
	params := named.Values()

	client, err := getClient(context)
	if err != nil {
		return nil, err
	}

	outerLayers := client.OuterLayers()
	state := outerLayers.State()
	cursor := state.Cursor
	result := make(chan interface{})

	settings := []text.Setting{
		text.WithResult(result),
		text.WithPrompt(prompt),
		text.WithInline(
			geom.Vec2{R: cursor.R, C: cursor.C},
			state.Image.Size(),
		),
		text.WithParams(client.Params()),
	}

	if !params.Full {
		cursor := state.Cursor
		settings = append(
			settings,
			text.WithInline(
				geom.Vec2{R: cursor.R, C: cursor.C},
				state.Image.Size(),
			),
		)
	}

	if params.Reverse {
		settings = append(settings, text.WithReverse)
	}

	if (params.Animated == nil || (*params.Animated) == true) && client.Params().Animate() {
		var animations []anim.Creator
		for _, a := range client.Params().Animations() {
			if creator, ok := anim.Animations[a]; ok {
				animations = append(
					animations,
					creator,
				)
			}
		}

		// Add all of the defaults if the setting was empty
		if len(animations) == 0 {
			for _, creator := range anim.Animations {
				animations = append(
					animations,
					creator,
				)
			}
		}

		creator := animations[rand.Int()%len(animations)]
		settings = append(
			settings,
			text.WithAnimation(state.Image, creator),
		)
	}

	if params.Preset != nil {
		settings = append(
			settings,
			text.WithPreset(*params.Preset),
		)
	}

	if params.Placeholder != nil {
		settings = append(
			settings,
			text.WithPlaceholder(*params.Placeholder),
		)
	}

	if params.Single {
		settings = append(settings, text.WithSingle)
	}

	if client.Params().SkipInput() {
		return "test", nil
	}

	text := text.New(
		ctx,
		settings...,
	)

	outerLayers.NewLayer(
		text.Ctx(),
		text,
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
