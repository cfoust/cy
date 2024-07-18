package cy

import (
	"context"
	"time"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	S "github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/stories"
)

func createStoryServer(ctx context.Context) (cy *Cy, err error) {
	cy, err = Start(ctx, Options{
		Shell:      "/bin/bash",
		HideSplash: true,
	})
	if err != nil {
		return
	}
	return
}

func createStoryClient(ctx context.Context, cy *Cy) (client *Client, screen mux.Screen, err error) {
	client, err = cy.NewClient(ctx, ClientOptions{
		Env: map[string]string{
			"TERM":   "xterm-256color",
			"EDITOR": "/usr/bin/vim",
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

func createStory(ctx context.Context) (cy *Cy, client *Client, screen mux.Screen, err error) {
	cy, err = createStoryServer(ctx)
	if err != nil {
		return
	}

	client, screen, err = createStoryClient(ctx, cy)
	return
}

var initWithFrame stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	_, _, screen, err := createStory(ctx)
	return screen, err
}

var initNoFrame stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	_, client, screen, err := createStory(ctx)
	client.execute(`(viewport/set-size [0 0])`)
	return screen, err
}

func init() {
	stories.Register("cy/viewport", initWithFrame, stories.Config{
		Input: []interface{}{
			stories.Wait(stories.Some),
			stories.Type("ctrl+a", "g"),
			stories.Wait(stories.More),
			stories.Type("ctrl+a", "g"),
			stories.Wait(stories.More),
			stories.Type("ctrl+a", "+"),
			stories.Wait(stories.Some),
			stories.Type("ctrl+a", "-"),
			stories.Wait(stories.More),
		},
	})

	stories.Register("cy/replay", initNoFrame, stories.Config{
		Input: []interface{}{
			stories.Wait(stories.Some),
			stories.Type("this is a test", "enter"),
			stories.Wait(stories.More),
			stories.Type("seq 1 10", "enter"),
			stories.Wait(stories.More),
			stories.Type("ctrl+a", "p"),
			stories.Wait(stories.Some),
			stories.Type("left", "left", "left", "left"),
			stories.Wait(stories.ALot),
			stories.Type("space"),
			stories.Wait(stories.ALot),
			stories.Type("?", "test", "enter"),
			stories.Wait(stories.ALot),
			stories.Type("h", "h", "h"),
			stories.Wait(stories.ALot),
		},
	})

	stories.Register("cy/shell", initNoFrame, stories.Config{
		Input: []interface{}{
			stories.Wait(stories.Some),
			stories.Type("ls -lah", "enter"),
			stories.Wait(stories.More),
			stories.Type("this is the first shell"),
			stories.Wait(stories.More),
			stories.Type("ctrl+a", "j"),
			stories.Wait(stories.More),
			stories.Type("this is a new shell"),
			stories.Wait(stories.ALot),
			stories.Type("ctrl+l"),
			stories.Wait(stories.Some),
			stories.Type("ctrl+l"),
			stories.Wait(stories.ALot),
		},
	})

	stories.Register("cy/project", initNoFrame, stories.Config{
		Input: []interface{}{
			stories.Wait(stories.Some),
			stories.Type("mkdir -p test-dir", "enter"),
			stories.Wait(stories.More),
			stories.Type("cd test-dir", "enter"),
			stories.Wait(stories.More),
			stories.Type("ctrl+a", "n"),
			stories.Wait(stories.ALot),
			stories.Type("ctrl+l"),
			stories.Wait(stories.Some),
			stories.Type("ctrl+l"),
			stories.Wait(stories.ALot),
		},
	})

	stories.Register("cy/switch-shells", initNoFrame, stories.Config{
		Input: []interface{}{
			stories.Wait(stories.Some),
			stories.Type("mkdir -p test-dir", "enter"),
			stories.Wait(stories.More),
			stories.Type("cd test-dir", "enter"),
			stories.Wait(stories.More),
			stories.Type("ctrl+a", "n"),
			stories.Wait(stories.ALot),
			stories.Type("ctrl+l"),
			stories.Wait(stories.Some),
			stories.Type("ctrl+l"),
			stories.Wait(stories.ALot),
			stories.Type("ctrl+a", "j"),
			stories.Wait(stories.Some),
			stories.Type("this is a new shell"),
			stories.Wait(stories.ALot),
			stories.Type("ctrl+a", ";"),
			stories.Wait(stories.More),
			stories.Type("down"),
			stories.Wait(stories.Some),
			stories.Type("down"),
			stories.Wait(stories.Some),
			stories.Type("up"),
			stories.Wait(stories.ALot),
			stories.Type("testshell", "down"),
			stories.Wait(stories.ALot),
			stories.Type("enter"),
			stories.Wait(stories.ALot),
		},
	})

	stories.Register("cy/palette", initWithFrame, stories.Config{
		Input: []interface{}{
			stories.Wait(stories.Some),
			stories.Type("ctrl+a", "ctrl+p"),
			stories.Wait(stories.More),
			stories.Type("Choose"),
			stories.Wait(stories.Some),
			stories.Type("enter"),
			stories.Wait(stories.Some),
			stories.Type("down"),
			stories.Wait(stories.Some),
			stories.Type("down"),
			stories.Wait(stories.Some),
			stories.Type("enter"),
			stories.Wait(stories.ALot),
		},
	})

	stories.Register("cy/palette-static", func(ctx context.Context) (mux.Screen, error) {
		_, client, screen, err := createStory(ctx)
		go client.execute(`(action/command-palette)`)
		return screen, err
	}, stories.Config{})

	stories.Register("cy/multiple-clients", func(ctx context.Context) (mux.Screen, error) {
		cy, err := createStoryServer(ctx)
		if err != nil {
			return nil, err
		}

		_, screenA, err := createStoryClient(ctx, cy)
		if err != nil {
			return nil, err
		}

		_, screenB, err := createStoryClient(ctx, cy)
		if err != nil {
			return nil, err
		}

		split := S.NewSplit(
			ctx,
			screenA,
			screenB,
			.5,
			false,
		)

		go func() {
			proportion := 0

			for {
				if ctx.Err() != nil {
					return
				}
				split.SetPercent(0.2 + float64(proportion)*0.1)

				time.Sleep(time.Second)
				proportion++
				if proportion >= 6 {
					proportion = 0
				}
			}
		}()

		return split, err
	}, stories.Config{})

	stories.Register("replay/command/time-jump", initReplay, stories.Config{
		Input: []interface{}{
			stories.Wait(stories.Some),
			stories.Type("[c"),
			stories.Wait(stories.Some),
			stories.Type("[c"),
			stories.Wait(stories.Some),
			stories.Type("[c"),
			stories.Wait(stories.Some),
			stories.Type("]c"),
			stories.Wait(stories.ALot),
			stories.Type("space"),
			stories.Wait(stories.ALot),
		},
	})

	stories.Register("replay/command/copy-jump", initReplay, stories.Config{
		Input: []interface{}{
			stories.Wait(stories.Some),
			stories.Type("k"),
			stories.Wait(stories.More),
			stories.Type("[c"),
			stories.Wait(stories.Some),
			stories.Type("[c"),
			stories.Wait(stories.Some),
			stories.Type("[c"),
			stories.Wait(stories.Some),
			stories.Type("]c"),
			stories.Wait(stories.ALot),
		},
	})

	stories.Register("replay/command/copy-jump-and-copy", initReplay, stories.Config{
		Input: []interface{}{
			stories.Wait(stories.Some),
			stories.Type("k"),
			stories.Wait(stories.More),
			stories.Type("[C"),
			stories.Wait(stories.Some),
			stories.Type("[C"),
			stories.Wait(stories.Some),
			stories.Type("[C"),
			stories.Wait(stories.Some),
			stories.Type("]C"),
			stories.Wait(stories.ALot),
		},
	})

	stories.Register("replay/time-demo", initReplay, stories.Config{
		Input: []interface{}{
			stories.Wait(stories.Some),
			stories.Type("gg"),
			stories.Wait(stories.Some),
			stories.Type("space"),
			stories.Wait(stories.Some),
			stories.Type("!"),
			stories.Wait(stories.Some),
			stories.Type("space"),
			stories.Wait(stories.ALot),
			stories.Wait(stories.ALot),
		},
	})

	stories.Register("replay/time-demo-search", initReplay, stories.Config{
		Input: []interface{}{
			stories.Wait(stories.Some),
			stories.Type("?"),
			stories.Wait(stories.Some),
			stories.Type("tolstoy"),
			stories.Wait(stories.Some),
			stories.Type("enter"),
			stories.Wait(stories.ALot),
		},
	})

	stories.Register("replay/time-demo-search-time", initReplay, stories.Config{
		Input: []interface{}{
			stories.Wait(stories.Some),
			stories.Type("?"),
			stories.Wait(stories.Some),
			stories.Type("1s"),
			stories.Wait(stories.Some),
			stories.Type("enter"),
			stories.Wait(stories.ALot),
		},
	})

}
