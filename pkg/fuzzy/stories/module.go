package stories

import (
	"context"

	"github.com/cfoust/cy/pkg/fuzzy"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/stories"
)

var BottomRight stories.InitFunc = func(ctx context.Context) mux.Screen {
	location := geom.DEFAULT_SIZE
	location.C -= 1
	location.R -= 1
	return fuzzy.NewFuzzy(
		ctx,
		pokemon,
		fuzzy.WithInline(location, geom.DEFAULT_SIZE),
	)
}

var TopLeft stories.InitFunc = func(ctx context.Context) mux.Screen {
	return fuzzy.NewFuzzy(
		ctx,
		pokemon,
		fuzzy.WithInline(geom.Size{}, geom.DEFAULT_SIZE),
	)
}

var Search stories.InitFunc = func(ctx context.Context) mux.Screen {
	f := fuzzy.NewFuzzy(
		ctx,
		pokemon,
		fuzzy.WithInline(geom.Size{}, geom.DEFAULT_SIZE),
	)

	stories.Send(f, "Pid")
	return f
}

var FullTop stories.InitFunc = func(ctx context.Context) mux.Screen {
	f := fuzzy.NewFuzzy(
		ctx,
		pokemon,
		fuzzy.WithReverse,
	)
	return f
}

var FullBottom stories.InitFunc = func(ctx context.Context) mux.Screen {
	f := fuzzy.NewFuzzy(
		ctx,
		pokemon,
	)
	return f
}

func init() {
	config := stories.Config{
		Size: geom.DEFAULT_SIZE,
	}
	stories.Register("input/find/bottom-right", BottomRight, config)
	stories.Register("input/find/top-left", TopLeft, config)
	stories.Register("input/find/search", Search, config)
	stories.Register("input/find/full-top", FullTop, config)
	stories.Register("input/find/full-bottom", FullBottom, config)
}
