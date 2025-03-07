package rasterion

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"

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
func triangleArea(v0, v1, v2 gl.Vec2) float32 {
	return .5 * ((v1[1]-v0[1])*(v1[0]+v0[0]) + (v2[1]-v1[1])*(v2[0]+v1[0]) + (v0[1]-v2[1])*(v0[0]+v2[0]))
}

var StaticShader = func(uv gl.Vec3) emu.Glyph {
	c := emu.EmptyGlyph()
	c.Char = '*'
	return c
}

func Triangle(
	i image.Image,
	s Shader,
	v0, v1, v2 gl.Vec2,
) {
	size := i.Size()
	if size.IsZero() {
		return
	}

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
	boundMin[1] = max(0, min(boundMin[1], v1[1], v1[1], v2[1]))
	boundMax[0] = min(clamp[0], max(boundMax[0], v0[0], v1[0], v2[0]))
	boundMax[1] = min(clamp[1], max(boundMax[1], v1[1], v1[1], v2[1]))

	boundMini := geom.Vec2{
		R: int(boundMin[1]),
		C: int(boundMin[0]),
	}
	boundMaxi := geom.Vec2{
		R: int(boundMax[1]),
		C: int(boundMax[0]),
	}

	var (
		totalArea          = triangleArea(v0, v1, v2)
		p                  gl.Vec2
		alpha, beta, gamma float32
	)
	for row := boundMini.R; row <= boundMaxi.R; row++ {
		for col := boundMini.C; col <= boundMaxi.C; col++ {
			// Center the point in the cell
			p[0] = float32(col) + 0.5
			p[1] = float32(row) + 0.5

			// Barymetric coordinates
			alpha = triangleArea(p, v1, v2) / totalArea
			beta = triangleArea(p, v2, v0) / totalArea
			gamma = triangleArea(p, v0, v1) / totalArea

			// negative => point not in triangle
			if alpha < 0 || beta < 0 || gamma < 0 {
				continue
			}

			i[row][col] = s(gl.Vec3{alpha, beta, gamma})
		}
	}
}
