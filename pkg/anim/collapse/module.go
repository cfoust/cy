package collapse

import (
	"time"

	"github.com/cfoust/cy/pkg/anim/meta"
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom/image"
)

const (
	TICKS_PER_SECOND = 10
)

type Collapse struct {
	last    time.Duration
	current image.Image
}

var _ meta.Animation = (*Collapse)(nil)

func (c *Collapse) Init(start image.Image) {
	c.current = start
}

func (c *Collapse) Update(delta time.Duration) image.Image {
	if (delta - c.last) < (time.Second / TICKS_PER_SECOND) {
		return c.current
	}

	c.last = delta

	size := c.current.Size()

	for row := size.R - 1; row >= 0; row-- {
		for col := size.C - 1; col >= 0; col-- {
			// can't go past bottom
			if row+1 == size.R {
				continue
			}

			next := c.current[row+1][col]
			if !next.IsEmpty() {
				continue
			}
			c.current[row+1][col] = c.current[row][col]
			c.current[row][col] = emu.EmptyGlyph()
		}
	}

	return c.current
}
