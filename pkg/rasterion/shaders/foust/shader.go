package foust

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	R "github.com/cfoust/cy/pkg/rasterion"

	gl "github.com/go-gl/mathgl/mgl32"
)

type foustSymbol struct {
	Rune rune
	Cell foustCell
}

type FoustShader struct {
	cells [][]foustCell
	size  geom.Size
}

var _ R.LineFragmentShader = (*FoustShader)(nil)

func (f *FoustShader) Fragment(
	gl_FragCoord gl.Vec2,
	i0, i1 int,
	v0, v1 gl.Vec3,
	t float32,
) (glyph emu.Glyph, discard bool) {
	glyph = emu.EmptyGlyph()
	glyph.Char = '*'
	col := int(gl_FragCoord[0])
	row := int(gl_FragCoord[1])
	newValue := f.cells[row][col].Draw(v0.Vec2(), v1.Vec2())
	f.cells[row][col] = newValue
	glyph.Char = newValue.Query()
	return
}

func (f *FoustShader) Size() geom.Vec2 {
	return f.size
}

func (f *FoustShader) Clear() {
	size := f.size
	for row := 0; row < size.R; row++ {
		for col := 0; col < size.C; col++ {
			f.cells[row][col] = foustCell(0)
		}
	}
}

func (f *FoustShader) Resize(size geom.Vec2) {
	var cells [][]foustCell = make([][]foustCell, size.R)
	for row := 0; row < size.R; row++ {
		cells[row] = make([]foustCell, size.C)
	}
	f.cells = cells
	f.size = size
}

func NewFoustShader(size geom.Size) *FoustShader {
	c := &FoustShader{}
	c.Resize(size)
	return c
}
