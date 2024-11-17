package perlin

import (
	"math"
	"math/rand"
	"time"

	"github.com/cfoust/cy/pkg/anim/meta"
	"github.com/cfoust/cy/pkg/anim/shader"
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom/image"
)

const (
	speed = 0.05
)

var (
	texture    = []rune(".,-:;)(cyCY")
	numTexture = len(texture)
)

var perlinShader shader.Shader = func(in image.Image) shader.ShaderOutput {
	p := NewPerlin(2.0, 2.0, 8, time.Now().UnixNano())

	xDelta := rand.Float64() - 0.5
	yDelta := rand.Float64() - 0.5

	return func(
		ts time.Duration,
		row int, col int,
		rows int, cols int,
	) emu.Glyph {
		progress := (ts.Seconds() + 5) / 10

		if progress > 1 {
			progress = 1
		}

		// Easing function for smooth animation
		easedProgress := -(math.Cos(math.Pi*progress) - 1) / 2
		easedProgress = math.Pow(easedProgress, 2)

		easedProgress = 1 - easedProgress

		g := in[row][col]
		x := float64(col) / float64(cols)
		x += ts.Seconds() * speed * xDelta
		y := float64(row) / float64(rows)
		y += ts.Seconds() * speed * yDelta
		value := p.Noise2D(x, y) + 0.2
		value += easedProgress * 2
		index := int(value * float64(numTexture))

		if index >= 0 && index < numTexture {
			g.Char = texture[index]
			g.BG = emu.DefaultBG
			g.FG = emu.DefaultFG
		}

		return g
	}
}

func New() meta.Animation {
	return shader.NewShaderAnimation(perlinShader)
}
