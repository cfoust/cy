package rasterion

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"

	gl "github.com/go-gl/mathgl/mgl32"
)

type Context struct {
	camera *Camera
	i      image.Image
	z      []float32
	size   geom.Vec2
}

func allocZ(size geom.Size) []float32 {
	return make([]float32, size.R*size.C)
}

func (c *Context) Camera() *Camera {
	return c.camera
}

func (c *Context) getIndex(row, col int) int {
	return col + row*c.size.C
}

func (c *Context) getZ(row, col int) float32 {
	return c.z[c.getIndex(row, col)]
}

func (c *Context) setZ(row, col int, value float32) {
	c.z[c.getIndex(row, col)] = value
}

func (c *Context) Image() image.Image {
	return c.i
}

func (c *Context) Resize(size geom.Vec2) {
	c.i = image.New(size)
	c.camera = NewCamera(size)
	c.z = allocZ(size)
	c.size = size
}

func (c *Context) Clear() {
	e := emu.EmptyGlyph()
	size := c.i.Size()

	for row := 0; row < size.R; row++ {
		for col := 0; col < size.C; col++ {
			c.i[row][col] = e
			c.z[c.getIndex(row, col)] = gl.InfPos
		}
	}
}

func New(size geom.Size) *Context {
	c := &Context{}
	c.Resize(size)
	return c
}

type Drawable interface {
	Draw(c *Context)
}
