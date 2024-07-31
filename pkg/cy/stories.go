package cy

import (
	"context"
	"time"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/layout"
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
		emu.WithoutHistory,
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
	stories.Register("quick-start/layout", func(ctx context.Context) (
		mux.Screen,
		error,
	) {
		_, client, screen, err := createStory(ctx)
		client.execute(`
(def cmd (cmd/new :root))
(layout/set {:type :pane :id cmd :attached true})
		`)
		return screen, err
	}, stories.Config{
		Input: []interface{}{
			stories.Wait(stories.Some),
			stories.Type("ctrl+a", "|"),
			stories.Wait(stories.Some),
			stories.Type("ctrl+a", "-"),
			stories.Wait(stories.Some),
			stories.Type("ctrl+a", "K"),
			stories.Wait(stories.Some),
			stories.Type("ctrl+a", "H"),
			stories.Wait(stories.More),
		},
	})

	stories.Register("quick-start/margins", initWithFrame, stories.Config{
		Input: []interface{}{
			stories.Wait(stories.Some),
			stories.Type("ctrl+a", "g"),
			stories.Wait(stories.More),
			stories.Type("ctrl+a", "g"),
			stories.Wait(stories.More),
			stories.Type("ctrl+a", "1"),
			stories.Wait(stories.More),
			stories.Type("ctrl+a", "2"),
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
		if err != nil {
			return nil, err
		}
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

		split := layout.NewSplit(
			ctx,
			screenA,
			screenB,
			false,
		)

		go func() {
			proportion := 0

			for {
				if ctx.Err() != nil {
					return
				}
				split.SetPercent(20 + proportion*10)

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

	stories.Register("toasts", func(ctx context.Context) (
		mux.Screen,
		error,
	) {
		_, client, screen, err := createStory(ctx)
		client.execute(`
(msg/toast :info "this shows up in blue")
(msg/toast :warn "this shows up in yellow")
(msg/toast :error "this shows up in red")
		`)
		return screen, err
	}, stories.Config{
		Size: geom.Size{R: 15, C: 60},
	})

	stories.Register("logs", func(ctx context.Context) (
		mux.Screen,
		error,
	) {
		server, client, screen, err := createStory(ctx)
		logs, _ := server.tree.Root().ChildByName("logs")
		client.Attach(logs)
		client.execute(`
(msg/log :info "this shows up in green")
(msg/log :warn "this shows up in red(ish?)")
(msg/log :error "this shows up in red")
		`)
		return screen, err
	}, stories.Config{
		Size: geom.Size{R: 5, C: 80},
	})

	stories.Register("fluid-fuzzy", func(ctx context.Context) (
		mux.Screen,
		error,
	) {
		_, client, screen, err := createStory(ctx)
		client.execute(`
(viewport/set-frame "puzzle")
(param/set :root :animations @["fluid"])
		`)
		return screen, err
	}, stories.Config{
		Size: geom.Size{
			R: 20,
			C: 120,
		},
		Input: []interface{}{
			stories.Wait(stories.Some),
			stories.Type("ctrl+a", ";"),
			stories.Wait(stories.ALot),
		},
	})

	stories.Register("input", initWithFrame, stories.Config{
		Input: []interface{}{
			stories.Wait(stories.Some),
			stories.Type("ctrl+a", "ctrl+p"),
			stories.Wait(stories.Some),
			stories.Type("Rename"),
			stories.Wait(stories.Some),
			stories.Type("enter"),
			stories.Wait(stories.Some),
			stories.Type("backspace"),
			stories.Wait(stories.Some),
			stories.Type("new-shell-name"),
			stories.Wait(stories.Some),
			stories.Type("enter"),
			stories.Wait(stories.ALot),
		},
	})

	stories.Register("layout/split-half", func(ctx context.Context) (
		mux.Screen,
		error,
	) {
		_, client, screen, err := createStory(ctx)
		client.execute(`
(def cmd1 (shell/new))
(def cmd2 (shell/new))
(layout/set
        {:type :split
         :percent 26
	 :a {:type :pane :id cmd1 :attached true}
	 :b {:type :pane :id cmd2}})
		`)
		return screen, err
	}, stories.Config{})

	stories.Register("layout/split-half-top", func(ctx context.Context) (
		mux.Screen,
		error,
	) {
		_, client, screen, err := createStory(ctx)
		client.execute(`
(def cmd1 (shell/new))
(def cmd2 (shell/new))
(def cmd3 (shell/new))
(layout/set
        {:type :split
         :percent 50
	 :vertical true
	 :a {:type :split
		 :percent 50
		 :a {:type :pane :id cmd1}
		 :b {:type :pane :id cmd2}}
	 :b {:type :pane :id cmd3 :attached true}})
		`)
		return screen, err
	}, stories.Config{})

	stories.Register("layout/split-half-cells", func(ctx context.Context) (
		mux.Screen,
		error,
	) {
		_, client, screen, err := createStory(ctx)
		client.execute(`
(def cmd1 (shell/new))
(def cmd2 (shell/new))
(layout/set
        {:type :split
         :cells 30
	 :a {:type :pane :id cmd1 :attached true}
	 :b {:type :pane :id cmd2}})
		`)
		return screen, err
	}, stories.Config{})

	stories.Register("layout/split-margins", func(ctx context.Context) (
		mux.Screen,
		error,
	) {
		_, client, screen, err := createStory(ctx)
		client.execute(`
(def cmd1 (shell/new))
(def cmd2 (shell/new))
(layout/set
        {:type :split
         :percent 80
	 :a {:type :margins
	     :cols 40
	     :node {:type :pane :id cmd1 :attached true}}
	 :b {:type :pane :id cmd2}})
		`)
		return screen, err
	}, stories.Config{})

	stories.Register("command/success", func(ctx context.Context) (
		mux.Screen,
		error,
	) {
		_, client, screen, err := createStory(ctx)
		client.execute(`
(def cmd (cmd/new :root :command "bash" :args ["-c" "exit 0"]))
(pane/attach cmd)
		`)
		return screen, err
	}, stories.Config{})

	stories.Register("command/fail", func(ctx context.Context) (
		mux.Screen,
		error,
	) {
		_, client, screen, err := createStory(ctx)
		client.execute(`
(def cmd (cmd/new :root :command "bash" :args ["-c" "exit 128"]))
(pane/attach cmd)
		`)
		return screen, err
	}, stories.Config{})

	stories.Register("pane/killed", func(ctx context.Context) (
		mux.Screen,
		error,
	) {
		_, client, screen, err := createStory(ctx)
		client.execute(`
(def cmd (cmd/new :root))
(layout/set {:type :pane :id cmd :attached true})
(tree/kill cmd)
		`)
		return screen, err
	}, stories.Config{})

	stories.Register("pane/invalid-id", func(ctx context.Context) (
		mux.Screen,
		error,
	) {
		_, client, screen, err := createStory(ctx)
		client.execute(`
(layout/set {:type :pane :id 1234 :attached true})
		`)
		return screen, err
	}, stories.Config{})

	stories.Register("pane/invalid-group", func(ctx context.Context) (
		mux.Screen,
		error,
	) {
		_, client, screen, err := createStory(ctx)
		client.execute(`
(def group (group/mkdir :root "/foo/bar/baz"))
(layout/set {:type :pane :id group :attached true})
		`)
		return screen, err
	}, stories.Config{})
}
