package rasterion

import (
	"github.com/cfoust/cy/pkg/geom"

	gl "github.com/go-gl/mathgl/mgl32"
)

const (
	// this corrects for the fact that terminal cells are not squares
	aspect = 0.5017144097222223
)

type Camera struct {
	size             geom.Vec2
	View, Projection gl.Mat4
}

func (c *Camera) Viewport() geom.Vec2 {
	return c.size
}

func (c *Camera) Transform(in gl.Vec3) gl.Vec4 {
	return c.Projection.
		Mul4(c.View).
		Mul4x1(in.Vec4(1))
}

// Project transforms the given point in screen-space into world-space.
func (c *Camera) UnProject(in gl.Vec3) (gl.Vec3, error) {
	return gl.UnProject(
		in,
		c.View,
		c.Projection,
		0, 0,
		c.size.C, c.size.R,
	)
}

func NewCamera(size geom.Vec2) *Camera {
	return &Camera{
		size: size,
		Projection: gl.Perspective(
			gl.DegToRad(45.0),
			float32(size.C)/(float32(size.R)/aspect),
			0.1,
			10.0,
		),
		View: gl.LookAtV(
			gl.Vec3{0, 0, 5},
			gl.Vec3{0, 0, 0},
			gl.Vec3{0, 1, 0},
		),
	}
}
