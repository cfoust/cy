package main

import (
	"fmt"
	"image"
	"image/draw"
	"image/gif"
	"image/png"
	"os"
)

// extractLastFrame decodes a GIF and saves its last frame as a PNG.
func extractLastFrame(gifFile, outputFile string) error {
	f, err := os.Open(gifFile)
	if err != nil {
		return fmt.Errorf("opening gif: %w", err)
	}
	defer f.Close()

	g, err := gif.DecodeAll(f)
	if err != nil {
		return fmt.Errorf("decoding gif: %w", err)
	}

	if len(g.Image) == 0 {
		return fmt.Errorf("gif has no frames")
	}

	// GIF frames may be partial updates (disposal method). Build
	// the composite of all frames up to the last one.
	bounds := image.Rect(
		0, 0, g.Config.Width, g.Config.Height,
	)
	canvas := image.NewRGBA(bounds)

	for _, frame := range g.Image {
		draw.Draw(
			canvas,
			frame.Bounds(),
			frame,
			frame.Bounds().Min,
			draw.Over,
		)
	}

	out, err := os.Create(outputFile)
	if err != nil {
		return fmt.Errorf("creating output: %w", err)
	}
	defer out.Close()

	return png.Encode(out, canvas)
}
