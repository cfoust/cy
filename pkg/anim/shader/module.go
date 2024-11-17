package shader

import (
	"time"

	"github.com/cfoust/cy/pkg/anim/meta"
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom/image"
)

// ShaderOutput is a function that calculates the output for a single screen
// cell.
type ShaderOutput func(
	ts time.Duration,
	row int,
	col int,
	rows int,
	cols int,
) emu.Glyph

// Shader takes an initial screen state and returns a function that
// calculates the output for each screen cell at each time step.
type Shader func(image.Image) ShaderOutput

// ShaderAnimation is a simple system for writing OpenGL-like "shaders".
type ShaderAnimation struct {
	in     image.Image
	out    image.Image
	shader Shader
	output ShaderOutput
}

var _ meta.Animation = (*ShaderAnimation)(nil)

func (s *ShaderAnimation) Init(start image.Image) {
	s.in = start.Clone()
	s.out = image.New(start.Size())
	s.output = s.shader(s.in)
}

func (s *ShaderAnimation) Update(delta time.Duration) image.Image {
	size := s.out.Size()
	for row := 0; row < size.R; row++ {
		for col := 0; col < size.C; col++ {
			s.out[row][col] = s.output(delta, row, col, size.R, size.C)
		}
	}

	return s.out
}

func NewShaderAnimation(shader Shader) *ShaderAnimation {
	return &ShaderAnimation{
		shader: shader,
	}
}
