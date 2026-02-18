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
	return New(
		ctx,
		pokemonText,
		WithInline(location, geom.DEFAULT_SIZE),
	), nil
}

var TopLeft stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	return New(
		ctx,
		pokemonText,
		WithInline(geom.Size{}, geom.DEFAULT_SIZE),
	), nil
}

var Search stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	f := New(
		ctx,
		pokemonText,
		WithInline(geom.Size{}, geom.DEFAULT_SIZE),
	)

	stories.Send(f, "Pid")
	return f, nil
}

var FullTop stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	f := New(
		ctx,
		pokemonText,
		WithReverse,
	)
	return f, nil
}

var FullBottom stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	f := New(
		ctx,
		pokemonText,
	)
	return f, nil
}

var FullTopTable stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	f := New(
		ctx,
		pokemonTable,
		WithReverse,
		WithHeaders("Name", "#", "Type"),
	)
	return f, nil
}

var FullBottomTable stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	f := New(
		ctx,
		pokemonTable,
		WithHeaders("Name", "#", "Type"),
	)
	return f, nil
}

var TopLeftTable stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	return New(
		ctx,
		pokemonTable,
		WithHeaders("Name", "#", "Type"),
		WithInline(geom.Size{}, geom.DEFAULT_SIZE),
	), nil
}

var CommandTable stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	return New(
		ctx,
		[]Option{
			newColumnOption("ll", "/foo/bar"),
			newColumnOption("ll", "/foo/bar"),
			newColumnOption("longer command", "/foo/bar"),
		},
		WithInline(geom.Size{}, geom.DEFAULT_SIZE),
	), nil
}

var Wrap stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	return New(
		ctx,
		[]Option{
			NewOption("apple", 0),
			NewOption("banana", 1),
			NewOption("cherry", 2),
			NewOption("date", 3),
			NewOption("elderberry", 4),
		},
	), nil
}

func init() {
	config := stories.Config{
		Size: geom.DEFAULT_SIZE,
		Input: []interface{}{
			stories.Type("ctrl+j"),
			stories.Wait(stories.Some),
			stories.Type("ctrl+j"),
			stories.Wait(stories.Some),
			stories.Type("ctrl+k"),
			stories.Wait(stories.Some),
			stories.Type("ctrl+k"),
			stories.Wait(stories.Some),
			stories.Type("B"),
			stories.Wait(stories.Some),
			stories.Type("u"),
			stories.Wait(stories.Some),
			stories.Type("t"),
			stories.Wait(stories.ALot),
		},
	}
	stories.Register("input/find/bottom-right", BottomRight, config)
	stories.Register("input/find/top-left", TopLeft, config)
	stories.Register("input/find/search", Search, config)
	stories.Register("input/find/full-top", FullTop, config)
	stories.Register("input/find/full-bottom", FullBottom, config)
	stories.Register("input/find/empty", func(
		ctx context.Context,
	) (mux.Screen, error) {
		f := New(ctx, []Option{})
		return f, nil
	}, config)

	scrollingInputs := []interface{}{}
	for i := 0; i < 150; i++ {
		scrollingInputs = append(
			scrollingInputs,
			stories.Type("ctrl+k"),
			stories.Wait(stories.ABit),
		)
	}
	scrolling := stories.Config{
		Size: geom.Size{
			R: 5,
			C: 80,
		},
		Input: scrollingInputs,
	}
	stories.Register("input/find/scroll/full-bottom", FullBottom, scrolling)

	stories.Register("input/find/table/full-top", FullTopTable, config)
	stories.Register("input/find/table/full-bottom", FullBottomTable, config)
	stories.Register("input/find/table/top-left", TopLeftTable, config)
	stories.Register("input/find/table/command", CommandTable, config)

	wrapInputs := []interface{}{}
	// Navigate up past the last item to wrap around
	for i := 0; i < 7; i++ {
		wrapInputs = append(
			wrapInputs,
			stories.Type("ctrl+k"),
			stories.Wait(stories.Some),
		)
	}
	// Then back down past the first to wrap the other way
	for i := 0; i < 7; i++ {
		wrapInputs = append(
			wrapInputs,
			stories.Type("ctrl+j"),
			stories.Wait(stories.Some),
		)
	}
	stories.Register("input/find/wrap", Wrap, stories.Config{
		Size: geom.Size{
			R: 10,
			C: 40,
		},
		Input: wrapInputs,
	})
}
