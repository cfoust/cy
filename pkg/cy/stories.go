package cy

import (
	"context"
	"time"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	S "github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/stories"
)

func createStory(ctx context.Context) (cy *Cy, client *Client, screen mux.Screen, err error) {
	cy, err = Start(ctx, Options{Shell: "/bin/bash"})
	if err != nil {
		return
	}

	client, err = cy.NewClient(ctx, ClientOptions{
		Env: map[string]string{
			"TERM": "xterm-256color",
		},
		Size: geom.DEFAULT_SIZE,
	})
	if err != nil {
		return
	}

	screen = S.NewTerminal(
		ctx,
		client,
		geom.DEFAULT_SIZE,
	)

	return
}

var SplashStory stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	_, _, screen, err := createStory(ctx)
	return screen, err
}

func init() {
	stories.Register("cy/clear-splash", SplashStory, stories.Config{
		Input: []interface{}{
			stories.Wait(time.Second),
			"q",
			stories.Wait(time.Second),
			"test",
			"ctrl+a",
			"p",
			stories.Wait(10 * time.Second),
		},
	})
}
