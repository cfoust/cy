package anim

import (
	"unicode"

	"github.com/cfoust/cy/pkg/geom/image"
)

type Animation interface {
	// delta is [0..1], indicates progress through animation
	Update(delta float32) image.Image
}

type CyFade struct {
	start image.Image
}

var _ Animation = (*CyFade)(nil)

func Fade(start image.Image) Animation {
	return &CyFade{
		start: start,
	}
}

func (c *CyFade) Update(delta float32) image.Image {
	output := c.start.Clone()

	end := 'a' + int32(delta*25)
	mapping := make(map[rune]rune)
	for i := 'a'; i < end; i++ {
		target := 'c'
		if (i % 2) == 1 {
			target = 'y'
		}

		mapping[i] = target
		mapping[unicode.ToUpper(i)] = unicode.ToUpper(target)
	}

	size := output.Size()
	for y := 0; y < size.Rows; y++ {
		for x := 0; x < size.Columns; x++ {
			current := c.start[y][x]

			if mapped, ok := mapping[current.Char]; ok {
				current.Char = mapped
				current.FG = c.start[y][x].FG
			}

			output[y][x] = current
		}
	}

	return output
}
