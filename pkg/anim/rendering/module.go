package rendering

import (
	"math"
	"time"

	"github.com/cfoust/cy/pkg/anim/meta"
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom/image"
	R "github.com/cfoust/cy/pkg/rasterion"

	gl "github.com/go-gl/mathgl/mgl32"
)

const (
	TICKS_PER_SECOND = 30
)

type LineTest struct {
	start time.Time
	last  time.Duration
	rCtx  *R.Context
}

var _ meta.Animation = (*LineTest)(nil)

func (c *LineTest) Init(start image.Image) {
	c.start = time.Now()
	c.rCtx = R.New(start.Size())
}

var shader R.Shader = func(uv gl.Vec3) emu.Glyph {
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
	current := c.rCtx.Image()
	if (delta - c.last) < (time.Second / TICKS_PER_SECOND) {
		return current
	}

	c.last = delta

	t := time.Now().Sub(c.start).Seconds()

	r := c.rCtx
	r.Camera.View = gl.LookAtV(
		gl.Vec3{0, 0, (float32(math.Sin(t)+1.)/2. * 5.) + .5},
		gl.Vec3{0, 0, 0},
		gl.Vec3{0, 1, 0},
	)
	r.Camera.View = gl.LookAtV(
		gl.Vec3{(float32(math.Sin(t))/2. * 5.) + .5, 3, 3},
		gl.Vec3{0, 0, 0},
		gl.Vec3{0, 1, 0},
	)
	r.Clear()

	for _, loc := range []struct{
		Start, Size float32
	}{
		{0, 1},
		{2, 1},
		{-2, 1},
	} {
		start, size := loc.Start, loc.Size
		r.Triangle(
			shader,
			r.Transform(gl.Vec3{start, 0, 0}),
			r.Transform(gl.Vec3{start+size, 0, 0}),
			r.Transform(gl.Vec3{start+size, size, 0}),
		)

		r.Triangle(
			shader,
			r.Transform(gl.Vec3{start+size, size, 0}),
			r.Transform(gl.Vec3{start, size, 0}),
			r.Transform(gl.Vec3{start, 0, 0}),
		)

	}

	return current
}
