//go:build stories
// +build stories

package replay

import (
	"context"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/stories"
	"github.com/xo/terminfo"
)

var Smoke stories.Story = func(ctx context.Context) mux.Screen {
	replay := New(
		ctx,
		sessions.NewSimulator().
			Add(
				"\033[20h", // CRLF -- why is this everywhere?
				geom.DEFAULT_SIZE,
				"test string please ignore",
			).
			Term(terminfo.ClearScreen).
			Add("take two").
			Term(terminfo.ClearScreen).
			Add("test").
			Events(),
		bind.NewBindScope(),
	)

	return replay
}

func init() {
	config := stories.Config{
		Size: geom.DEFAULT_SIZE,
	}
	stories.Register("smoke", Smoke, config)
}
