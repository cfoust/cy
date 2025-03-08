package rasterion

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"

	gl "github.com/go-gl/mathgl/mgl32"
)

type Camera struct {
	View       gl.Mat4
	Projection gl.Mat4
}

type Context struct {
	Camera
	i image.Image
	z []float32
}

func allocZ(size geom.Size) []float32 {
	return make([]float32, size.R*size.C)
}

func (c *Context) getZ(row, col int) float32 {
	return c.z[row*col]
}

func (c *Context) setZ(row, col int, value float32) {
	c.z[row*col] = value
}

func (c *Context) Image() image.Image {
	return c.i
}

func (c *Context) Clear() {
	e := emu.EmptyGlyph()
	size := c.i.Size()

	for row := 0; row < size.R; row++ {
		for col := 0; col < size.C; col++ {
			c.i[row][col] = e
			c.z[row*col] = gl.InfNeg
		}
	}
}

func (c *Context) Transform(in gl.Vec3) gl.Vec3 {
	size := c.i.Size()
	return gl.Project(
		in,
		c.View,
		c.Projection,
		0, 0,
		size.C, size.R,
	)
}

func New(size geom.Size) *Context {
	i := image.New(size)
	return &Context{
		i: i,
		z: allocZ(size),
		Camera: Camera{
			Projection: gl.Perspective(
				gl.DegToRad(45.0),
				float32(size.C)/float32(size.R),
				0.1,
				10.0,
			),
			View: gl.LookAtV(
				gl.Vec3{0, 0, 5},
				gl.Vec3{0, 0, 0},
				gl.Vec3{0, 1, 0},
			),
		},
	}
}
