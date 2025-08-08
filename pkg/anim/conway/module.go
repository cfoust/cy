package conway

import (
	"time"

	"github.com/cfoust/cy/pkg/anim/meta"
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom/image"
)

type Conway struct {
	last  time.Duration
	image image.Image
}

var _ meta.Animation = (*Conway)(nil)

func (c *Conway) Init(start image.Image) {
	c.image = start.Clone()
}

const (
	TICKS_PER_SECOND = 10
)

func isAlive(glyph emu.Glyph) bool {
	return glyph.Char != ' '
}

func reproduce(neighbors []emu.Glyph) emu.Glyph {
	// TODO(cfoust): 09/11/23 make this more interesting
	for _, neighbor := range neighbors {
		if isAlive(neighbor) {
			return neighbor
		}
	}

	return emu.EmptyGlyph()
}

func (c *Conway) Update(delta time.Duration) image.Image {
	if (delta - c.last) < (time.Second / TICKS_PER_SECOND) {
		return c.image
	}

	c.last = delta
	next := c.image.Clone()

	size := c.image.Size()

	neighbors := make([]emu.Glyph, 9)
	var dx, dy, cellR, cellC, numAlive int
	var isCellAlive bool
	for row := 0; row < size.R; row++ {
		for col := 0; col < size.C; col++ {
			numAlive = 0
			isCellAlive = isAlive(c.image[row][col])

			for neighbor := 0; neighbor < 9; neighbor++ {
				neighbors[neighbor] = emu.EmptyGlyph()
				dx = (neighbor % 3) - 1
				dy = (neighbor / 3) - 1
				if dx == 0 && dy == 0 {
					continue
				}

				cellR = row + dy
				cellC = col + dx
				if cellR < 0 || cellR >= size.R || cellC < 0 ||
					cellC >= size.C {
					continue
				}

				cell := c.image[cellR][cellC]
				if isAlive(cell) {
					numAlive++
					neighbors[neighbor] = cell
				}
			}

			if !isCellAlive {
				if numAlive == 3 {
					next[row][col] = reproduce(neighbors)
				}
				continue
			}

			if numAlive == 2 || numAlive == 3 {
				continue
			}

			next[row][col] = emu.EmptyGlyph()
		}
	}

	c.image = next
	return c.image
}
