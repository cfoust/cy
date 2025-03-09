package rasterion

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"

	gl "github.com/go-gl/mathgl/mgl32"
)

type Shader func(uv gl.Vec3) emu.Glyph

func min(vals ...float32) float32 {
	if len(vals) == 0 {
		return 0
	}

	var value = gl.MaxValue
	for _, v := range vals {
		if v > value {
			continue
		}
		value = v
	}

	return value
}

func max(vals ...float32) float32 {
	if len(vals) == 0 {
		return 0
	}

	var value = gl.MinValue
	for _, v := range vals {
		if v < value {
			continue
		}
		value = v
	}

	return value
}

// triangleArea computes the signed area of the given triangle.
func triangleArea(v0, v1, v2 gl.Vec3) float32 {
	return .5 * ((v1[1]-v0[1])*(v1[0]+v0[0]) + (v2[1]-v1[1])*(v2[0]+v1[0]) + (v0[1]-v2[1])*(v0[0]+v2[0]))
}

var StaticShader = func(uv gl.Vec3) emu.Glyph {
	c := emu.EmptyGlyph()
	c.Char = '*'
	return c
}

func (c *Context) Triangle(s Shader, v0, v1, v2 gl.Vec3) {
	size := c.i.Size()

	// First compute the bounding box for the triangle in screen space
	var (
		boundMin = gl.Vec2{
			float32(size.C - 1),
			float32(size.R - 1),
		}
		boundMax = gl.Vec2{}
		clamp    = boundMin
	)
	boundMin[0] = max(0, min(boundMin[0], v0[0], v1[0], v2[0]))
	boundMin[1] = max(0, min(boundMin[1], v0[1], v1[1], v2[1]))
	boundMax[0] = min(clamp[0], max(boundMax[0], v0[0], v1[0], v2[0]))
	boundMax[1] = min(clamp[1], max(boundMax[1], v0[1], v1[1], v2[1]))

	boundMini := geom.Vec2{
		R: int(boundMin[1]),
		C: int(boundMin[0]),
	}
	boundMaxi := geom.Vec2{
		R: int(boundMax[1]),
		C: int(boundMax[0]),
	}

	var (
		totalArea = triangleArea(v0, v1, v2)
		p, bary   gl.Vec3
		z         = gl.Vec3{v0[2], v1[2], v2[2]}
	)

	// Backface culling
	if totalArea < 0 {
		return
	}

	for row := boundMini.R; row <= boundMaxi.R; row++ {
		for col := boundMini.C; col <= boundMaxi.C; col++ {
			// Center the point in the cell
			p[0] = float32(col) + 0.5
			p[1] = float32(row) + 0.5

			// Barymetric coordinates
			bary[0] = triangleArea(p, v1, v2) / totalArea
			bary[1] = triangleArea(p, v2, v0) / totalArea
			bary[2] = triangleArea(p, v0, v1) / totalArea

			// Negative => point not in triangle
			if bary[0] < 0 || bary[1] < 0 || bary[2] < 0 {
				continue
			}

			// Interpolate Z-value
			p[2] = z.Dot(bary)

			// Depth
			if p[2] < c.getZ(row, col) {
				continue
			}

			c.i[size.R-1-row][col] = s(bary)
			c.setZ(row, col, p[2])
		}
	}
}

func (c *Context) Triangles(s Shader, faces [][3]gl.Vec3) {
	for _, face := range faces {
		c.Triangle(s, face[0], face[1], face[2])
	}
}

func (c *Context) TriangleStrip(s Shader, vertices []gl.Vec3) {
	if len(vertices) < 3 {
		return
	}

	for i := 0; i <= len(vertices)-3; i++ {
		// To maintain winding order
		// See https://www.khronos.org/opengl/wiki/Primitive
		// TODO(cfoust): 03/09/25 fix winding order
		if (i % 2) == 0 {
			c.Triangle(
				s,
				vertices[i+1],
				vertices[i],
				vertices[i+2],
			)
			continue
		}

		c.Triangle(
			s,
			vertices[i],
			vertices[i+1],
			vertices[i+2],
		)
	}
}
