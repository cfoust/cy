package placeholder

import (
	"context"

	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/stories"
)

var Placeholder stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	return screen.AddMargins(ctx, New(ctx)), nil
}

func init() {
	stories.Register("placeholder", Placeholder, stories.Config{})
}
