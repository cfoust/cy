package rendering

import (
	"time"
	"math"

	"github.com/cfoust/cy/pkg/anim/meta"
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/rasterion"

	gl "github.com/go-gl/mathgl/mgl32"
)

const (
	TICKS_PER_SECOND = 30
)

type LineTest struct {
	start   time.Time
	last    time.Duration
	current image.Image
}

var _ meta.Animation = (*LineTest)(nil)

func (c *LineTest) Init(start image.Image) {
	c.start = time.Now()
	c.current = image.New(start.Size())
}

var shader = func(uv gl.Vec3) emu.Glyph {
	c := emu.EmptyGlyph()
	c.Char = '*'
	c.FG = emu.RGBColor(
		int(uv[0]*255),
		int(uv[1]*255),
		int(uv[2]*255),
	)
	return c
}

func (c *LineTest) Update(delta time.Duration) image.Image {
	if (delta - c.last) < (time.Second / TICKS_PER_SECOND) {
		return c.current
	}

	c.last = delta

	size := c.current.Size()
	c.current.Clear(
		geom.Rect{Size: size},
	)

	t := time.Now().Sub(c.start).Seconds()

	rasterion.Triangle(
		c.current,
		shader,
		gl.Vec2{},
		gl.Vec2{float32(size.C), 0},
		gl.Vec2{
			float32(((math.Cos(t) + 1) / 2) * float64(size.C)),
			float32(size.R),
		},
	)

	return c.current
}
