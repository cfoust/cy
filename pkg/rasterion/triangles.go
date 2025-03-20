package rasterion

import (
	"math"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"

	gl "github.com/go-gl/mathgl/mgl32"
)

func min(vals ...float32) float32 {
	if len(vals) == 0 {
		return 0
	}

	var value = gl.InfPos
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

	var value = gl.InfNeg
	for _, v := range vals {
		if v < value {
			continue
		}
		value = v
	}

	return value
}

// triangleArea computes the signed area of the given triangle.
func triangleArea(v0, v1, v2 gl.Vec2) float32 {
	return .5 * ((v1[1]-v0[1])*(v1[0]+v0[0]) + (v2[1]-v1[1])*(v2[0]+v1[0]) + (v0[1]-v2[1])*(v0[0]+v2[0]))
}

var StaticShader = func(uv gl.Vec3) emu.Glyph {
	c := emu.EmptyGlyph()
	c.Char = '*'
	return c
}

func barycentric(v0, v1, v2, P gl.Vec2) gl.Vec3 {
	var (
		s0 = gl.Vec3{
			v2[0] - v0[0],
			v1[0] - v0[0],
			v0[0] - P[0],
		}
		s1 = gl.Vec3{
			v2[1] - v0[1],
			v1[1] - v0[1],
			v0[1] - P[1],
		}
		u = s0.Cross(s1)
	)

	if math.Abs(float64(u[2])) < 1e-2 {
		return gl.Vec3{-1, 1, 1}
	}

	return gl.Vec3{
		1 - (u[0]+u[1])/u[2],
		u[1] / u[2],
		u[0] / u[2],
	}
}

func normalizeDeviceCoordinates(v gl.Vec4) gl.Vec4 {
	v[3] = 1 / v[3]

	for i := 0; i < 3; i++ {
		v[i] *= v[3]
	}

	return v
}

func (c *Context) transformPoint(
	s VertexShader,
	viewport gl.Vec2,
	face, index int,
	point gl.Vec3,
) (transformed, window gl.Vec4, behind bool) {
	transformed = s.Vertex(c.camera, face, index, point)

	// First transform to NDC
	window = normalizeDeviceCoordinates(transformed)

	// Then convert to window coordinates
	for j := 0; j < 2; j++ {
		window[j] = viewport[j] * 0.5 * (window[j] + 1.0)
	}

	behind = window.Z() < 0 || window.W() < 0
	return
}

func (c *Context) triangle(
	s Shader,
	face, i0, i1, i2 int,
	p0, p1, p2 gl.Vec3,
) {
	var (
		size     = c.i.Size()
		viewport = gl.Vec2{
			float32(size.C),
			float32(size.R),
		}
		v0, w0, b0 = c.transformPoint(s, viewport, face, 0, p0)
		v1, w1, b1 = c.transformPoint(s, viewport, face, 1, p1)
		v2, w2, b2 = c.transformPoint(s, viewport, face, 2, p2)
	)

	// Do not draw triangles behind the camera
	if b0 || b1 || b2 {
		return
	}

	var (
		v = []gl.Vec4{w0, w1, w2}
	)

	// First compute the bounding box for the triangle in screen space
	var (
		boundMin = gl.Vec2{
			gl.InfPos,
			gl.InfPos,
		}
		boundMax = gl.Vec2{
			gl.InfNeg,
			gl.InfNeg,
		}
		clamp = gl.Vec2{
			float32(size.C - 1),
			float32(size.R - 1),
		}
	)
	boundMin[0] = min(v[0][0], v[1][0], v[2][0])
	boundMin[1] = min(v[0][1], v[1][1], v[2][1])
	boundMax[0] = max(boundMax[0], v[0][0], v[1][0], v[2][0])
	boundMax[1] = max(boundMax[1], v[0][1], v[1][1], v[2][1])

	// Exclude triangles that are not on the screen
	if boundMax[0] < 0 || boundMin[0] > clamp[0] {
		return
	}

	if boundMax[1] < 0 || boundMin[1] > clamp[1] {
		return
	}

	boundMin[0] = max(boundMin[0], 0.)
	boundMin[1] = max(boundMin[1], 0.)

	boundMax[0] = min(boundMax[0], clamp[0])
	boundMax[1] = min(boundMax[1], clamp[1])

	boundMini := geom.Vec2{
		R: int(boundMin[1]),
		C: int(boundMin[0]),
	}
	boundMaxi := geom.Vec2{
		R: int(boundMax[1]),
		C: int(boundMax[0]),
	}

	var (
		totalArea = triangleArea(
			v[0].Vec2(),
			v[1].Vec2(),
			v[2].Vec2(),
		)
		oneOverZ = gl.Vec3{
			1 / v0[2],
			1 / v1[2],
			1 / v2[2],
		}
		p          gl.Vec4
		baryScreen gl.Vec3
		z          = gl.Vec3{v[0][2], v[1][2], v[2][2]}
		w          = gl.Vec3{v[0][3], v[1][3], v[2][3]}
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

			// Barycentric coordinates
			baryScreen = barycentric(
				v[0].Vec2(),
				v[1].Vec2(),
				v[2].Vec2(),
				p.Vec2(),
			)

			// Negative => point not in triangle
			if baryScreen[0] < 0 || baryScreen[1] < 0 || baryScreen[2] < 0 {
				continue
			}

			// Perform perspective correction on barycentric coords
			invZ := baryScreen[0]*oneOverZ[0] + baryScreen[1]*oneOverZ[1] + baryScreen[2]*oneOverZ[2]
			baryClip := gl.Vec3{
				baryScreen[0] * oneOverZ[0] / invZ,
				baryScreen[1] * oneOverZ[1] / invZ,
				baryScreen[2] * oneOverZ[2] / invZ,
			}

			// Interpolate Z- and W-values
			p[2] = z.Dot(baryClip)
			p[3] = w.Dot(baryClip)

			// Depth
			if p[2] > c.getZ(row, col) {
				continue
			}

			char, discard := s.Fragment(i0, i1, i2, baryClip)
			if discard {
				continue
			}

			c.i[size.R-1-row][col] = char
			c.setZ(row, col, p[2])
		}
	}
}

func (c *Context) Triangle(s Shader, v0, v1, v2 gl.Vec3) {
	c.triangle(
		s,
		0, 0, 1, 2,
		v0, v1, v2,
	)
}

func (c *Context) Triangles(
	s Shader,
	verts []gl.Vec3,
	faces [][3]int,
) {
	var i0, i1, i2 int
	for face, faceVerts := range faces {
		i0, i1, i2 = faceVerts[0], faceVerts[1], faceVerts[2]
		c.triangle(
			s,
			face, i0, i1, i2,
			verts[i0], verts[i1], verts[i2],
		)
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
			c.triangle(
				s,
				i, i+1, i, i+2,
				vertices[i+1],
				vertices[i],
				vertices[i+2],
			)
			continue
		}

		c.triangle(
			s,
			i, i, i+1, i+2,
			vertices[i],
			vertices[i+1],
			vertices[i+2],
		)
	}
}
