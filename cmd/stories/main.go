package main

import (
	"context"
	"fmt"
	"io"
	"os"
	"runtime"
	"time"

	"github.com/cfoust/cy/pkg/anim"
	_ "github.com/cfoust/cy/pkg/cy"
	"github.com/cfoust/cy/pkg/frames"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/stream/cli"
	"github.com/cfoust/cy/pkg/mux/stream/renderer"
	"github.com/cfoust/cy/pkg/rasterion/opengl"
	_ "github.com/cfoust/cy/pkg/rasterion/stories"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/stories"
	"github.com/cfoust/cy/pkg/stories/ui"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/alecthomas/kong"
	"github.com/rs/zerolog/log"
	"github.com/xo/terminfo"
	"golang.org/x/term"
)

var CLI struct {
	Prefix string `help:"Pre-filter the list of stories." name:"prefix" optional:"" short:"p"`
	Single string `help:"Show a single story in full screen, overriding its config." name:"single" optional:"" short:"s"`
	Cast   string `help:"Save an asciinema cast to the given filename. Must be used in tandem with --single." name:"cast" optional:"" short:"c"`
	Width  int    `help:"Set the width of the terminal when saving an asciinema cast." name:"width" optional:"" short:"w" default:"80"`
	Height int    `help:"Set the height of the terminal when saving an asciinema cast." name:"height" optional:"" short:"h" default:"26"`
}

func main() {
	// Necessary for OpenGL
	runtime.LockOSThread()
	defer runtime.UnlockOSThread()

	kong.Parse(&CLI,
		kong.Name("cy-stories"),
		kong.Description("storybook, but for the CLI"),
		kong.UsageOnError(),
		kong.ConfigureHelp(kong.HelpOptions{
			Compact: true,
			Summary: true,
		}))

	logs, err := os.OpenFile(
		"stories.log",
		os.O_CREATE|os.O_APPEND|os.O_WRONLY,
		0644,
	)
	if err != nil {
		panic(err)
	}
	log.Logger = log.Output(logs)

	// Some stories use this
	opengl.DefaultRenderer = opengl.NewRenderer()

	// Start a goroutine to run the main stories app
	appDone := make(chan error, 1)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	go func() {
		defer cancel()
		appDone <- runStoriesApp(ctx)
	}()

	opengl.DefaultRenderer.Poll(ctx)

	select {
	case err := <-appDone:
		if err != nil {
			panic(err)
		}
	default:
	}
}

func runStoriesApp(ctx context.Context) error {
	for name, frame := range frames.Frames {
		func(f frames.Frame) {
			stories.Register(
				fmt.Sprintf("frame/%s", name),
				func(ctx context.Context) (mux.Screen, error) {
					framer := frames.NewFramer(
						ctx,
						f,
					)
					return framer, nil
				},
				stories.Config{},
			)
		}(frame)
	}

	for name, animation := range anim.Animations {
		func(a anim.Creator) {
			stories.Register(
				fmt.Sprintf("animation/%s", name),
				func(ctx context.Context) (mux.Screen, error) {
					return NewAnimationStory(
						ctx,
						a,
					), nil
				},
				stories.Config{},
			)
		}(animation)
	}

	// A stories story
	stories.Register(
		"stories",
		func(ctx context.Context) (mux.Screen, error) {
			return ui.New(ctx, CLI.Prefix)
		},
		stories.Config{
			Input: []interface{}{
				stories.Type("ctrl+j"),
				stories.Wait(stories.Some),
				stories.Type("ctrl+j"),
				stories.Wait(stories.Some),
				stories.Type("ctrl+k"),
				stories.Wait(stories.Some),
				stories.Type("ctrl+k"),
				stories.Wait(stories.Some),
				stories.Type("input"),
				stories.Type("ctrl+j"),
				stories.Wait(stories.Some),
				stories.Type("ctrl+j"),
				stories.Wait(stories.Some),
				stories.Type("ctrl+k"),
				stories.Wait(stories.Some),
				stories.Type("ctrl+k"),
				stories.Wait(stories.Some),
			},
		},
	)

	haveCast := len(CLI.Cast) > 0
	if len(CLI.Single) == 0 && haveCast {
		panic(fmt.Errorf("to use --cast, you must provide a single story"))
	}

	var cols, rows int
	var err error
	if haveCast {
		cols = CLI.Width
		rows = CLI.Height
	} else {
		cols, rows, err = term.GetSize(int(os.Stdin.Fd()))
		if err != nil {
			cols, rows = 80, 26
		}
	}

	size := geom.Size{C: cols, R: rows}

	info, err := terminfo.LoadFromEnv()
	if err != nil {
		panic(err)
	}

	appCtx := context.Background()

	var screen *taro.Program
	if len(CLI.Single) > 0 {
		story, ok := stories.Stories[CLI.Single]
		if !ok {
			panic(fmt.Errorf("story %s not found", CLI.Single))
		}

		screen = ui.NewViewer(
			appCtx,
			story,
			!haveCast,
		)

		if haveCast && !story.Config.HasInputs() {
			go func() {
				time.Sleep(5 * time.Second)
				screen.Cancel()
			}()
		}
	} else {
		screen, err = ui.New(appCtx, CLI.Prefix)
		if err != nil {
			panic(err)
		}
	}

	renderer := renderer.NewRenderer(screen.Ctx(), info, size, screen)

	if !haveCast {
		_ = cli.Attach(screen.Ctx(), renderer, os.Stdin, os.Stdout)
		return nil
	}

	recorder := sessions.NewMemoryRecorder()
	stream := sessions.NewEventStream(renderer, recorder)

	// The recorder only stores data on Read() calls, so we need to drain
	// it
	go func() { _, _ = io.Copy(io.Discard, stream) }()
	_ = stream.Resize(size)
	<-screen.Ctx().Done()

	err = sessions.WriteAsciinema(CLI.Cast, recorder.Events())
	if err != nil {
		return err
	}
	return nil
}
