package stories

import (
	"math"
	"time"

	"github.com/cfoust/cy/pkg/emu"
	R "github.com/cfoust/cy/pkg/rasterion"

	gl "github.com/go-gl/mathgl/mgl32"
)

type lineShader struct{}

var _ R.LineShader = (*lineShader)(nil)

func (l *lineShader) Vertex(
	camera *R.Camera, face, index int, vertex gl.Vec3,
) gl.Vec4 {
	return vertex.Vec4(1)
}

func (l *lineShader) Fragment(
	i0, i1 int,
	v0, v1 gl.Vec3,
	e0, e1 bool,
	t float32,
) (glyph emu.Glyph, discard bool) {
	glyph = emu.EmptyGlyph()
	glyph.Char = '#'
	glyph.FG = emu.RGBColor(
		int((1-t) * 255),
		0,
		0,
	)
	return
}

var s = &lineShader{}

var sinLine Drawing = func(c *R.Context, delta time.Duration) {
	t := delta.Seconds()
	c.Line(
		s,
		gl.Vec3{},
		gl.Vec3{
			float32(math.Cos(t)),
			float32(math.Sin(t)),
		},
	)
}
