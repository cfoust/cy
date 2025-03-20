package rasterion

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"

	gl "github.com/go-gl/mathgl/mgl32"
)

// line draws a line on the bitmap using Bresenham's algorithm.
func (c *Context) line(
	s LineShader,
	i0, i1 int,
	p0, p1 gl.Vec3,
) {
	var (
		image    = c.i
		size     = c.i.Size()
		viewport = gl.Vec2{
			float32(size.C),
			float32(size.R),
		}
		v0, w0, b0 = c.transformPoint(s, viewport, 0, 0, p0)
		v1, w1, b1 = c.transformPoint(s, viewport, 0, 1, p1)
	)

	if b0 || b1 {
		return
	}

	var (
		boundMin = gl.Vec2{
			min(w0[0], w1[0]),
			min(w0[1], w1[1]),
		}
		boundMax = gl.Vec2{
			max(w0[0], w1[0]),
			max(w0[1], w1[1]),
		}
		clamp = gl.Vec2{
			float32(size.C - 1),
			float32(size.R - 1),
		}
	)

	// Exclude lines that are not on the screen
	if boundMax[0] < 0 || boundMin[0] > clamp[0] {
		return
	}

	if boundMax[1] < 0 || boundMin[1] > clamp[1] {
		return
	}

	var (
		x0       = int(gl.Clamp(w0[0], 0, clamp[0]))
		y0       = int(gl.Clamp(w0[1], 0, clamp[1]))
		x1       = int(gl.Clamp(w1[0], 0, clamp[0]))
		y1       = int(gl.Clamp(w1[1], 0, clamp[1]))
		oneOverZ = gl.Vec3{
			1 / v0[2],
			1 / v1[2],
		}
	)

	var (
		line    = w1.Sub(w0)
		length  = line.Len()
		p       gl.Vec4
		glyph   emu.Glyph
		discard bool
		isStart bool
		isEnd   bool
		t       float32
		invZ    float32
	)

	line = line.Normalize()

	if length == 0 {
		return
	}

	isStart = true

	dx := geom.Abs(x1 - x0)
	dy := geom.Abs(y1 - y0)

	sx := -1
	if x0 < x1 {
		sx = 1
	}
	sy := -1
	if y0 < y1 {
		sy = 1
	}
	err := dx - dy

	for {
		isEnd = x0 == x1 && y0 == y1

		// Center the point in the cell
		p[0] = float32(x0) + 0.5
		p[1] = float32(y0) + 0.5

		// Calculate distance along line
		t = gl.Clamp(
			line.Dot(p.Sub(w0))/length,
			0, 1,
		)

		// Correct for perspective
		invZ = (1-t)*oneOverZ[0] + t*oneOverZ[1]
		t = (1 - t) * oneOverZ[0] / invZ

		glyph, discard = s.Fragment(
			i0, i1,
			gl.Vec3{}, gl.Vec3{},
			isStart, isEnd, t,
		)

		if !discard {
			image[y0][x0] = glyph
		}

		if isEnd {
			break
		}

		isStart = false

		e2 := 2 * err
		if e2 > -dy {
			err -= dy
			x0 += sx
		}
		if e2 < dx {
			err += dx
			y0 += sy
		}
	}
}

func (c *Context) Line(s LineShader, v0, v1 gl.Vec3) {
	c.line(s, 0, 1, v0, v1)
}
