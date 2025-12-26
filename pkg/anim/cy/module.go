package cy

import (
	"time"
	"unicode"

	"github.com/cfoust/cy/pkg/anim/meta"
	"github.com/cfoust/cy/pkg/geom/image"
)

type Cyform struct {
	initial image.Image
	current image.Image
}

var _ meta.Animation = (*Cyform)(nil)

func (c *Cyform) Init(start image.Image) {
	c.initial = start
	c.current = start.Clone()
}

func (c *Cyform) Update(delta time.Duration) image.Image {
	elapsed := delta.Seconds()
	if elapsed > 1.0 {
		return c.current
	}

	var (
		end     = 'a' + int32(elapsed*25)
		mapping = make(map[rune]rune)
	)
	for i := 'a'; i < end; i++ {
		target := 'c'
		if (i % 2) == 1 {
			target = 'y'
		}

		mapping[i] = target
		mapping[unicode.ToUpper(i)] = unicode.ToUpper(target)
	}

	size := c.current.Size()
	for row := 0; row < size.R; row++ {
		for col := 0; col < size.C; col++ {
			current := c.initial[row][col]

			if mapped, ok := mapping[current.Char]; ok {
				current.Char = mapped
				current.FG = c.current[row][col].FG
			}

			c.current[row][col] = current
		}
	}

	return c.current
}
