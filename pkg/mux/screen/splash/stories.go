package splash

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/stories"
)

var SplashStory stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	return New(
		ctx,
		geom.DEFAULT_SIZE,
		true,
	), nil
}

func init() {
	stories.Register("splash", SplashStory, stories.Config{
		Size: geom.DEFAULT_SIZE,
	})
}
