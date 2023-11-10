//go:build stories
// +build stories

package main

import (
	"context"
	"os"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux/stream/cli"
	"github.com/cfoust/cy/pkg/mux/stream/renderer"
	"github.com/cfoust/cy/pkg/stories"

	"github.com/xo/terminfo"
	"golang.org/x/term"
)

func isStories() bool {
	return true
}

func startStories() {
	ctx := context.Background()
	screen, err := stories.Initialize(ctx, "smoke")
	if err != nil {
		return
	}

	cols, rows, err := term.GetSize(int(os.Stdin.Fd()))
	if err != nil {
		return
	}

	info, err := terminfo.LoadFromEnv()
	if err != nil {
		return
	}

	raw := emu.New(emu.WithSize(geom.Vec2{
		R: rows,
		C: cols,
	}))
	renderer := renderer.NewRenderer(ctx, info, raw, screen)
	cli.Attach(ctx, renderer, os.Stdin, os.Stdout)
}
