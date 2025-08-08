package thumbs

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	R "github.com/cfoust/cy/pkg/rasterion"
	"github.com/cfoust/cy/pkg/rasterion/shaders/subcell"

	gl "github.com/go-gl/mathgl/mgl32"
)

type colorFragmentShader struct {
	FG, BG emu.Color
}

var _ R.LineFragmentShader = (*colorFragmentShader)(nil)

func (f *colorFragmentShader) Fragment(
	gl_FragCoord gl.Vec3,
	i0, i1 int,
	v0, v1 gl.Vec2,
	t float32,
) (glyph emu.Glyph, discard bool) {
	glyph.FG = f.FG
	glyph.BG = f.BG
	return
}

type shader struct {
	*R.NoopVertexShader
	*subcell.Shader
}

type lineDrawer struct {
	rast   *R.Context
	line   *subcell.Shader
	frag   *colorFragmentShader
	shader R.LineShader
}

func newLineDrawer() *lineDrawer {
	var (
		size = geom.DEFAULT_SIZE
		frag = &colorFragmentShader{
			FG: emu.DefaultFG,
			BG: emu.DefaultBG,
		}
	)

	l := &lineDrawer{
		rast: R.New(size),
		frag: frag,
		line: subcell.New(
			size,
			frag,
		),
	}

	l.shader = &shader{
		NoopVertexShader: &R.NoopVertexShader{},
		Shader:           l.line,
	}

	return l
}

func (l *lineDrawer) Prepare(size geom.Size) {
	if l.rast.Size() != size {
		l.rast.Resize(size)
	}

	l.rast.ClearZBuffer()
}

func (l *lineDrawer) SetTarget(i image.Image) {
	l.rast.SetTarget(i)
}

func (l *lineDrawer) Draw(
	from, to geom.Vec2,
	fg, bg emu.Color,
) {
	l.frag.FG = fg
	l.frag.BG = bg

	var (
		size = l.rast.Size()
		v0   = gl.Vec3{float32(from.C), float32(from.R)}
		v1   = gl.Vec3{float32(to.C), float32(to.R)}
	)

	for _, v := range []*gl.Vec3{&v0, &v1} {
		v[0] = ((v[0] / float32(size.C)) * 2) - 0.5
		v[1] = ((v[1] / float32(size.R)) * 2) - 0.5
		v[2] = 1
	}

	l.rast.Line(l.shader, v0, v1)
}
