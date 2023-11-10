//go:build stories
// +build stories

package main

import (
	"context"
	"os"
	"io"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux/stream/renderer"
	"github.com/cfoust/cy/pkg/stories"

	"github.com/muesli/termenv"
	"github.com/xo/terminfo"
	"golang.org/x/term"
)

func isStories() bool {
	return true
}

func pollRender(ctx context.Context, r *renderer.Renderer) {
	// TODO(cfoust): 07/16/23 replace with io.Copy
	buffer := make([]byte, 4096)

	for {
		numBytes, err := r.Read(buffer)
		if err == io.EOF {
			return
		}
		if err != nil {
			return
		}
		if ctx.Err() != nil {
			return
		}
		if numBytes == 0 {
			continue
		}

		os.Stdout.Write(buffer[:numBytes])
	}
}

func startStories() {
	output := termenv.NewOutput(os.Stdout)
	output.AltScreen()
	output.EnableMouseAllMotion()
	oldState, err := term.MakeRaw(int(os.Stdin.Fd()))
	if err != nil {
		return
	}
	defer func() {
		output.ExitAltScreen()
		output.DisableMouseAllMotion()
		term.Restore(int(os.Stdin.Fd()), oldState)
	}()

	info, err := terminfo.LoadFromEnv()
	if err != nil {
		return
	}

	cols, rows, err := term.GetSize(int(os.Stdin.Fd()))
	if err != nil {
		return
	}

	ctx := context.Background()
	screen, err := stories.Initialize(ctx, "smoke")
	if err != nil {
		return
	}

	raw := emu.New(emu.WithSize(geom.Vec2{
		R: rows,
		C: cols,
	}))

	renderer := renderer.NewRenderer(ctx, info, raw, screen)
	pollRender(ctx, renderer)
}
