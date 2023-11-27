package main

import (
	"context"
	"fmt"
	"os"

	"github.com/cfoust/cy/pkg/anim"
	"github.com/cfoust/cy/pkg/frames"
	_ "github.com/cfoust/cy/pkg/fuzzy/stories"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	_ "github.com/cfoust/cy/pkg/mux/screen/replay/stories"
	_ "github.com/cfoust/cy/pkg/mux/screen/splash/stories"
	"github.com/cfoust/cy/pkg/mux/stream/cli"
	"github.com/cfoust/cy/pkg/mux/stream/renderer"
	"github.com/cfoust/cy/pkg/stories"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/alecthomas/kong"
	"github.com/rs/zerolog/log"
	"github.com/xo/terminfo"
	"golang.org/x/term"
)

var CLI struct {
	Prefix string `help:"Pre-filter the list of stories." name:"prefix" optional:"" short:"p"`
	Single string `help:"Show a single story in full screen, overriding its config." name:"single" optional:"" short:"s"`
}

func main() {
	kong.Parse(&CLI,
		kong.Name("cy-stories"),
		kong.Description("storybook, but for the CLI"),
		kong.UsageOnError(),
		kong.ConfigureHelp(kong.HelpOptions{
			Compact: true,
			Summary: true,
		}))

	logs, err := os.OpenFile("stories.log", os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0644)
	if err != nil {
		panic(err)
	}
	log.Logger = log.Output(logs)

	ctx := context.Background()

	for name, frame := range frames.Frames {
		func(f frames.Frame) {
			stories.Register(
				fmt.Sprintf("frame/%s", name),
				func(ctx context.Context) mux.Screen {
					framer := frames.NewFramer(
						ctx,
						f,
					)
					return framer
				},
				stories.Config{},
			)
		}(frame)
	}

	for name, animation := range anim.Animations {
		func(a anim.Creator) {
			stories.Register(
				fmt.Sprintf("animation/%s", name),
				func(ctx context.Context) mux.Screen {
					return anim.NewStory(
						ctx,
						a,
					)
				},
				stories.Config{},
			)
		}(animation)
	}

	cols, rows, err := term.GetSize(int(os.Stdin.Fd()))
	if err != nil {
		panic(err)
	}

	info, err := terminfo.LoadFromEnv()
	if err != nil {
		panic(err)
	}

	var screen *taro.Program
	if len(CLI.Single) > 0 {
		story, ok := stories.Stories[CLI.Single]
		if !ok {
			panic(fmt.Errorf("story %s not found", CLI.Single))
		}
		screen = stories.NewViewer(
			ctx,
			story.Init(ctx),
			stories.Config{},
		)
	} else {
		screen, err = stories.Initialize(ctx, CLI.Prefix)
		if err != nil {
			panic(err)
		}
	}

	renderer := renderer.NewRenderer(
		screen.Ctx(),
		info,
		geom.Vec2{
			R: rows,
			C: cols,
		},
		screen,
	)
	cli.Attach(screen.Ctx(), renderer, os.Stdin, os.Stdout)
}
