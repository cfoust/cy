package cy

import (
	"time"
	"unicode"

	"github.com/cfoust/cy/pkg/anim/meta"
	"github.com/cfoust/cy/pkg/geom/image"
)

type Cyform struct {
	start image.Image
}

var _ meta.Animation = (*Cyform)(nil)

func (c *Cyform) Init(start image.Image) {
	c.start = start
}

func (c *Cyform) Update(delta time.Duration) image.Image {
	elapsed := delta.Seconds()
	if elapsed > 1.0 {
		return c.start
	}

	end := 'a' + int32(elapsed*25)
	mapping := make(map[rune]rune)
	for i := 'a'; i < end; i++ {
		target := 'c'
		if (i % 2) == 1 {
			target = 'y'
		}

		mapping[i] = target
		mapping[unicode.ToUpper(i)] = unicode.ToUpper(target)
	}

	size := c.start.Size()
	for y := 0; y < size.R; y++ {
		for x := 0; x < size.C; x++ {
			current := c.start[y][x]

			if mapped, ok := mapping[current.Char]; ok {
				current.Char = mapped
				current.FG = c.start[y][x].FG
			}

			c.start[y][x] = current
		}
	}

	return c.start
}
