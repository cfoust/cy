package overlay

import (
	"fmt"
	"image"
	"image/draw"
	"image/gif"
	"math"
	"os"
)

// GIF composites cursor sprites onto each frame of a GIF based on
// cursor events parsed from a cast file. When the cursor moves
// significantly during a single frame's display period, the frame
// is split into sub-frames for smooth motion.
func GIF(
	castFile, gifFile, outputFile string,
	fontSize, lineHeight, speed float64,
) error {
	timeline, err := newCursorTimeline(castFile, speed)
	if err != nil {
		return fmt.Errorf(
			"parsing cursor events: %w", err,
		)
	}
	if timeline.Empty() {
		data, err := os.ReadFile(gifFile)
		if err != nil {
			return err
		}
		return os.WriteFile(outputFile, data, 0644)
	}

	cursor := newCursorRenderer(fontSize, lineHeight)

	f, err := os.Open(gifFile)
	if err != nil {
		return fmt.Errorf("opening gif: %w", err)
	}
	defer f.Close()

	g, err := gif.DecodeAll(f)
	if err != nil {
		return fmt.Errorf("decoding gif: %w", err)
	}

	fullBounds := image.Rect(
		0, 0, g.Config.Width, g.Config.Height,
	)
	palette := ensureCursorColors(g.Image[0].Palette)
	canvas := image.NewRGBA(fullBounds)

	// Maximum cursor movement (in pixels) per output
	// sub-frame. ~0.5 cells gives web-like smoothness.
	const maxCursorStep = 6.0

	var (
		outImages    []*image.Paletted
		outDelays    []int
		outDisposal  []byte
		playbackTime float64
	)

	for i, frame := range g.Image {
		draw.Draw(
			canvas,
			frame.Bounds(),
			frame,
			frame.Bounds().Min,
			draw.Over,
		)

		delayCs := 0
		if i < len(g.Delay) {
			delayCs = g.Delay[i]
		}
		frameDelay := float64(delayCs) / 100.0

		startPos := timeline.At(playbackTime)
		endPos := timeline.At(playbackTime + frameDelay)

		subFrames := 1
		if startPos.Visible && endPos.Visible {
			dist := cursor.PixelDist(startPos, endPos)
			if dist > maxCursorStep {
				subFrames = int(math.Ceil(
					dist / maxCursorStep,
				))
			}
		}
		// Each sub-frame needs at least 2cs (the minimum
		// most GIF renderers respect).
		if maxSub := delayCs / 2; subFrames > maxSub &&
			maxSub > 0 {
			subFrames = maxSub
		}

		subDelays := distributeDelay(delayCs, subFrames)

		var prevPos cursorPos
		sfStart := playbackTime
		for sf := range subFrames {
			sfDelay := float64(subDelays[sf]) / 100.0
			pos := timeline.At(sfStart + sfDelay/2.0)

			var region image.Rectangle
			if sf == 0 {
				region = fullBounds
			} else {
				// Only repaint the area around
				// the old and new cursor positions.
				region = cursor.Rect(prevPos).
					Union(cursor.Rect(pos))
				region.Min.X--
				region.Min.Y--
				region.Max.X++
				region.Max.Y++
				region = region.Intersect(
					fullBounds,
				)
			}

			patch := image.NewRGBA(region)
			draw.Draw(
				patch, region,
				canvas, region.Min,
				draw.Src,
			)
			cursor.Draw(patch, pos)

			paletted := image.NewPaletted(
				region, palette,
			)
			draw.Draw(
				paletted, region,
				patch, region.Min,
				draw.Src,
			)

			outImages = append(outImages, paletted)
			outDelays = append(
				outDelays, subDelays[sf],
			)
			outDisposal = append(
				outDisposal, gif.DisposalNone,
			)

			prevPos = pos
			sfStart += sfDelay
		}

		playbackTime += frameDelay

		if i < len(g.Disposal) &&
			g.Disposal[i] == gif.DisposalBackground {
			draw.Draw(
				canvas,
				frame.Bounds(),
				image.Transparent,
				image.Point{},
				draw.Src,
			)
		}
	}

	g.Image = outImages
	g.Delay = outDelays
	g.Disposal = outDisposal

	out, err := os.Create(outputFile)
	if err != nil {
		return fmt.Errorf("creating output: %w", err)
	}
	defer out.Close()

	return gif.EncodeAll(out, g)
}

// distributeDelay splits a delay (in centiseconds) evenly across n
// sub-frames, ensuring each gets at least 2cs and the total is
// preserved.
func distributeDelay(totalCs, n int) []int {
	if n <= 1 {
		return []int{totalCs}
	}

	base := totalCs / n
	if base < 2 {
		result := make([]int, n)
		used := 0
		for i := range n - 1 {
			result[i] = 2
			used += 2
		}
		result[n-1] = max(totalCs-used, 2)
		return result
	}

	remainder := totalCs - base*n
	result := make([]int, n)
	for i := range result {
		result[i] = base
		if i < remainder {
			result[i]++
		}
	}
	return result
}
