package fuzzy

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/stories"
)

var BottomRight stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	location := geom.DEFAULT_SIZE
	location.C -= 1
	location.R -= 1
	return NewFuzzy(
		ctx,
		pokemonText,
		WithInline(location, geom.DEFAULT_SIZE),
	), nil
}

var TopLeft stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	return NewFuzzy(
		ctx,
		pokemonText,
		WithInline(geom.Size{}, geom.DEFAULT_SIZE),
	), nil
}

var Search stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	f := NewFuzzy(
		ctx,
		pokemonText,
		WithInline(geom.Size{}, geom.DEFAULT_SIZE),
	)

	stories.Send(f, "Pid")
	return f, nil
}

var FullTop stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	f := NewFuzzy(
		ctx,
		pokemonText,
		WithReverse,
	)
	return f, nil
}

var FullBottom stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	f := NewFuzzy(
		ctx,
		pokemonText,
	)
	return f, nil
}

var FullTopTable stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	f := NewFuzzy(
		ctx,
		pokemonTable,
		WithReverse,
		WithHeaders("Name", "Number", "Type"),
	)
	return f, nil
}

var FullBottomTable stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	f := NewFuzzy(
		ctx,
		pokemonTable,
		WithHeaders("Name", "Number", "Type"),
	)
	return f, nil
}

var TopLeftTable stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	return NewFuzzy(
		ctx,
		pokemonTable,
		WithHeaders("Name", "Number", "Type"),
		WithInline(geom.Size{}, geom.DEFAULT_SIZE),
	), nil
}

func init() {
	config := stories.Config{
		Size: geom.DEFAULT_SIZE,
		Input: []interface{}{
			stories.Type("But"),
			stories.Wait(stories.ALot),
		},
	}
	stories.Register("input/find/bottom-right", BottomRight, config)
	stories.Register("input/find/top-left", TopLeft, config)
	stories.Register("input/find/search", Search, config)
	stories.Register("input/find/full-top", FullTop, config)
	stories.Register("input/find/full-bottom", FullBottom, config)
	stories.Register("input/find/table/full-top", FullTopTable, config)
	stories.Register("input/find/table/full-bottom", FullBottomTable, config)
	stories.Register("input/find/table/top-left", TopLeftTable, config)
}
