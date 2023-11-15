package main

import (
	"context"
	"fmt"
	"os"

	"github.com/cfoust/cy/pkg/frames"
	_ "github.com/cfoust/cy/pkg/fuzzy/stories"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/mux"
	_ "github.com/cfoust/cy/pkg/mux/screen/replay/stories"
	"github.com/cfoust/cy/pkg/mux/stream/cli"
	"github.com/cfoust/cy/pkg/mux/stream/renderer"
	"github.com/cfoust/cy/pkg/stories"

	"github.com/alecthomas/kong"
	"github.com/rs/zerolog/log"
	"github.com/xo/terminfo"
	"golang.org/x/term"
)

var CLI struct {
	Prefix string `help:"Pre-filter the list of stories." name:"prefix" optional:"" short:"p"`
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

	for name, animation := range frames.Animations {
		func(a frames.Animation) {
			stories.Register(
				fmt.Sprintf("animation/%s", name),
				func(ctx context.Context) mux.Screen {
					animator := frames.NewAnimator(
						ctx,
						a,
						// TODO(cfoust): 11/17/23 replace with cy splash
						image.New(geom.DEFAULT_SIZE),
						23,
					)
					return animator
				},
				stories.Config{},
			)
		}(animation)
	}

	screen, err := stories.Initialize(ctx, CLI.Prefix)
	if err != nil {
		panic(err)
	}

	cols, rows, err := term.GetSize(int(os.Stdin.Fd()))
	if err != nil {
		panic(err)
	}

	info, err := terminfo.LoadFromEnv()
	if err != nil {
		panic(err)
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
