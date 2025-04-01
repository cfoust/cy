package subcell

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	R "github.com/cfoust/cy/pkg/rasterion"

	gl "github.com/go-gl/mathgl/mgl32"
)

type symbol struct {
	Rune rune
	Cell gridCell
}

type Shader struct {
	cells     [][]gridCell
	size      geom.Size
	subShader R.LineFragmentShader
}

var _ R.LineFragmentShader = (*Shader)(nil)

func (s *Shader) Fragment(
	gl_FragCoord gl.Vec3,
	i0, i1 int,
	v0, v1 gl.Vec2,
	t float32,
) (glyph emu.Glyph, discard bool) {
	glyph, discard = s.subShader.Fragment(
		gl_FragCoord,
		i0, i1,
		v0, v1,
		t,
	)
	if discard {
		return
	}

	col := int(gl_FragCoord[0])
	row := int(gl_FragCoord[1])
	// TODO(cfoust): 03/30/25 handle merging cells with same Z
	newValue := s.cells[row][col].Draw(v0, v1)
	s.cells[row][col] = newValue
	glyph.Char = newValue.Query()
	return
}

func (s *Shader) Size() geom.Vec2 {
	return s.size
}

func (s *Shader) Clear() {
	size := s.size
	for row := 0; row < size.R; row++ {
		for col := 0; col < size.C; col++ {
			s.cells[row][col] = gridCell(0)
		}
	}
}

func (s *Shader) Resize(size geom.Vec2) {
	var cells [][]gridCell = make([][]gridCell, size.R)
	for row := 0; row < size.R; row++ {
		cells[row] = make([]gridCell, size.C)
	}
	s.cells = cells
	s.size = size
}

func New(
	size geom.Size,
	shader R.LineFragmentShader,
) *Shader {
	c := &Shader{
		subShader: shader,
	}
	c.Resize(size)
	return c
}
