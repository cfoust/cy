package city

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom/image"
	R "github.com/cfoust/cy/pkg/rasterion"

	gl "github.com/go-gl/mathgl/mgl32"
)

var screenVerts = []gl.Vec3{
	{-1, 1, -1}, // Back-top-left
	{1, 1, -1},  // Back-top-right
	{-1, 1, 1},  // Front-top-left
	{1, 1, 1},   // Front-top-right
}

var screenUvs = []gl.Vec2{
	{0, 0},
	{1.0, 0},
	{0.0, 1.0},
	{1.0, 1.0},
}

var screenFaces = [][3]int{
	{2, 1, 0},
	{1, 2, 3},
}

type screenShader struct {
	R.TransformShader
	texture image.Image
}

var _ R.Shader = (*screenShader)(nil)
var _ R.Drawable = (*screenShader)(nil)

func (s *screenShader) Fragment(
	i0, i1, i2 int,
	bary gl.Vec3,
) (glyph emu.Glyph, discard bool) {
	m := gl.Mat2x3FromCols(
		screenUvs[i0],
		screenUvs[i1],
		screenUvs[i2],
	)
	uv := m.Mul3x1(bary)
	glyph = R.Texture(s.texture, uv)
	return
}

func (s *screenShader) Draw(c *R.Context) {
	c.Triangles(s, screenVerts, screenFaces)
}
