package stories

import (
	"math"
	"time"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	R "github.com/cfoust/cy/pkg/rasterion"
	"github.com/cfoust/cy/pkg/rasterion/shaders/subcell"

	gl "github.com/go-gl/mathgl/mgl32"
)

type noVertexShader struct{}

var _ R.VertexShader = (*noVertexShader)(nil)

func (v *noVertexShader) Vertex(
	camera *R.Camera, face, index int, vertex gl.Vec3,
) gl.Vec4 {
	return vertex.Vec4(1)
}

type lineShader struct{}

var _ R.LineFragmentShader = (*lineShader)(nil)

func (l *lineShader) Fragment(
	gl_FragCoord gl.Vec3,
	i0, i1 int,
	v0, v1 gl.Vec2,
	t float32,
) (glyph emu.Glyph, discard bool) {
	glyph = emu.EmptyGlyph()
	glyph.Char = '#'
	glyph.FG = emu.RGBColor(
		int((v0[0])*255),
		int((v1[0])*255),
		0,
	)
	return
}

var sinShader = &struct {
	*noVertexShader
	*lineShader
}{}

var sinLine Drawing = func(c *R.Context, delta time.Duration) {
	t := delta.Seconds()
	c.Line(
		sinShader,
		gl.Vec3{0, 0, 1},
		gl.Vec3{
			1 * float32(math.Cos(t)),
			1 * float32(math.Sin(t)),
			1,
		},
	)
}

var subcellShader = &struct {
	*noVertexShader
	*subcell.Shader
}{
	Shader: subcell.New(
		geom.DEFAULT_SIZE,
		sinShader,
	),
}

var subcellLine Drawing = func(c *R.Context, delta time.Duration) {
	size := c.Image().Size()
	if subcellShader.Size() != size {
		subcellShader.Resize(size)
	}

	subcellShader.Clear()

	t := delta.Seconds() / 5
	for i := 0; i < 5; i++ {
		advance := float64(i) * 0.1
		c.Line(
			subcellShader,
			gl.Vec3{0, 0, 1},
			gl.Vec3{
				1 * float32(math.Cos(t+advance)),
				1 * float32(math.Sin(t+advance)),
				1,
			},
		)
	}
}

func init() {
	registerStory("lines/sweep", sinLine)
	registerStory("lines/subcell", subcellLine)
}
