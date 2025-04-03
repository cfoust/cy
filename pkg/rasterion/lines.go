package rasterion

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"

	gl "github.com/go-gl/mathgl/mgl32"
)

// clampVector clamps the components of a Vec2 to the given range.
func clampVector(v gl.Vec2, low, high float32) gl.Vec2 {
	return gl.Vec2{
		gl.Clamp(v[0], low, high),
		gl.Clamp(v[1], low, high),
	}
}

// line draws a line on the bitmap using Bresenham's algorithm.
func (c *Context) line(
	s LineShader,
	segment, i0, i1 int,
	p0, p1 gl.Vec3,
) {
	var (
		image    = c.i
		size     = c.i.Size()
		viewport = gl.Vec2{
			float32(size.C),
			float32(size.R),
		}
		v0, w0, b0 = c.transformPoint(
			s,
			viewport,
			segment, i0, p0,
		)
		v1, w1, b1 = c.transformPoint(
			s,
			viewport,
			segment, i1, p1,
		)
		w0_2 = w0.Vec2()
		w1_2 = w1.Vec2()
	)

	if b0 || b1 {
		return
	}

	screen0, screen1, onScreen, _ := Rect{
		Size: gl.Vec2{
			float32(size.C - 1),
			float32(size.R - 1),
		},
	}.Intersections(w0_2, w1_2)

	// Exclude lines that are not on the screen
	if !onScreen {
		return
	}

	var (
		x0       = int(screen0[0])
		y0       = int(screen0[1])
		x1       = int(screen1[0])
		y1       = int(screen1[1])
		oneOverZ = gl.Vec3{
			1 / v0[2],
			1 / v1[2],
		}
	)

	var (
		line   = w1.Sub(w0)
		length = line.Len()
		p      gl.Vec4
		cell   = Rect{
			Size: gl.Vec2{1, 1},
		}
		glyph               emu.Glyph
		discard, intersects bool
		in0, in1            gl.Vec2
		t                   float32
		invZ                float32
	)

	line = line.Normalize()

	if length == 0 {
		return
	}

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
		// Center the point in the cell
		p[0] = float32(x0) + 0.5
		p[1] = float32(y0) + 0.5
		cell.Pos[0] = float32(x0)
		cell.Pos[1] = float32(y0)

		// Calculate distance along line
		t = gl.Clamp(
			line.Dot(p.Sub(w0))/length,
			0, 1,
		)

		// Correct for perspective
		invZ = t*oneOverZ[0] + (1-t)*oneOverZ[1]
		t = t * oneOverZ[0] / invZ

		// Finally, calculate Z
		p[2] = t*w0[2] + (1-t)*w1[2]

		if p[2] < c.getZ(y0, x0) {
			// We provide the fragment shader with the
			// intersections of the line we're drawing with the
			// current cell expressed as two vectors in the
			// coordinate frame of the cell. The components of
			// these vectors are between [0, 1].
			in0, in1, intersects, _ = cell.Intersections(
				w0_2,
				w1_2,
			)

			// Most of the time the line we're drawing intersects
			// with the cell the Bresenham algorithm is drawing.
			if intersects {
				in0 = in0.Sub(cell.Pos)
				in1 = in1.Sub(cell.Pos)
			} else {
				// But when it doesn't intersect, which can
				// happen, we pretend that the cell is closer
				// to the line than it actually is.
				nearest := w1.
					Sub(w0).
					Mul(1 - t).
					Add(w0).
					Vec2()
				// TODO(cfoust): 03/25/25 allow user to
				// configure fuzziness
				in0, in1, _, _ = Rect{
					Pos:  nearest,
					Size: gl.Vec2{1, 1},
				}.Intersections(w0_2, w1_2)
				in0 = in0.Sub(nearest)
				in1 = in1.Sub(nearest)
			}

			in0 = clampVector(in0, 0, 1)
			in1 = clampVector(in1, 0, 1)

			glyph, discard = s.Fragment(
				p.Vec3(),
				i0, i1,
				in0, in1,
				t,
			)
			if !discard {
				image[y0][x0] = glyph
				c.setZ(y0, x0, p[2])
			}
		}

		if x0 == x1 && y0 == y1 {
			break
		}

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

// Line draws a line in world space from v0 to v1 using the given shader.
func (c *Context) Line(s LineShader, v0, v1 gl.Vec3) {
	c.line(s, 0, 0, 1, v0, v1)
}

// Lines draws multiple line segments in world space using the given shader.
func (c *Context) Lines(
	s LineShader,
	verts []gl.Vec3,
	segments [][2]int,
) {
	var i0, i1 int
	for segment, segmentVerts := range segments {
		i0, i1 = segmentVerts[0], segmentVerts[1]
		c.line(
			s,
			segment, i0, i1,
			verts[i0], verts[i1],
		)
	}
}
