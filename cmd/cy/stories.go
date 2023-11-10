//go:build stories
// +build stories

package main

import (
	"context"
	"os"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux/stream/cli"
	"github.com/cfoust/cy/pkg/mux/stream/renderer"
	"github.com/cfoust/cy/pkg/stories"

	"github.com/rs/zerolog/log"
	"github.com/xo/terminfo"
	"golang.org/x/term"
)

func isStories() bool {
	return true
}

func startStories() {
	logs, err := os.OpenFile("stories.log", os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0644)
	if err != nil {
		panic(err)
	}
	log.Logger = log.Output(logs)

	ctx := context.Background()
	screen, err := stories.Initialize(ctx, "search-time-forward")
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
