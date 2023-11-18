package stories

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/screen/splash"
	"github.com/cfoust/cy/pkg/stories"
)

var Splash stories.InitFunc = func(ctx context.Context) mux.Screen {
	return splash.New(
		ctx,
		geom.DEFAULT_SIZE,
		true,
	)
}

func init() {
	stories.Register("splash", Splash, stories.Config{
		Size: geom.DEFAULT_SIZE,
	})
}
