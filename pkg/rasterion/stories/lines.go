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
	t float32,
) (glyph emu.Glyph, discard bool) {
	glyph = emu.EmptyGlyph()
	glyph.Char = '#'
	glyph.FG = emu.RGBColor(
		int((1-t)*255),
		int((t)*255),
		0,
	)
	return
}

var s = &lineShader{}

var sinLine Drawing = func(c *R.Context, delta time.Duration) {
	t := delta.Seconds()
	c.Line(
		s,
		gl.Vec3{0, 0, 1},
		gl.Vec3{
			2 * float32(math.Cos(t)),
			2 * float32(math.Sin(t)),
			1,
		},
	)
}

func init() {
	registerStory("lines/sweep", sinLine)
}
