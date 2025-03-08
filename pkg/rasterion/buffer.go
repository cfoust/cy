package rasterion

import (
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom/image"

	gl "github.com/go-gl/mathgl/mgl32"
)

type Buffer struct {
	i image.Image
	z []float32
}

func allocZ(size geom.Size) []float32 {
	return make([]float32, size.R*size.C)
}

func (b *Buffer) getZ(row, col int) float32 {
	return b.z[row * col]
}

func (b *Buffer) setZ(row, col int, value float32) {
	b.z[row * col] = value
}

func (b *Buffer) Image() image.Image {
	return b.i
}

func (b *Buffer) Clear() {
	e := emu.EmptyGlyph()
	size := b.i.Size()

	for row := 0; row < size.R; row++ {
		for col := 0; col < size.C; col++ {
			b.i[row][col] = e
			b.z[row * col] = gl.InfNeg
		}
	}
}

func New(size geom.Size) *Buffer {
	i := image.New(size)
	return &Buffer{
		i: i,
		z: allocZ(size),
	}
}
