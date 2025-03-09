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
	distance = 5
)

var cube = []gl.Vec3{
	{-1, 1, 1},   // Front-top-left
	{1, 1, 1},    // Front-top-right
	{-1, -1, 1},  // Front-bottom-left
	{1, -1, 1},   // Front-bottom-right
	{1, -1, -1},  // Back-bottom-right
	{1, 1, 1},    // Front-top-right
	{1, 1, -1},   // Back-top-right
	{-1, 1, 1},   // Front-top-left
	{-1, 1, -1},  // Back-top-left
	{-1, -1, 1},  // Front-bottom-left
	{-1, -1, -1}, // Back-bottom-left
	{1, -1, -1},  // Back-bottom-right
	{-1, 1, -1},  // Back-top-left
	{1, 1, -1},   // Back-top-right
}

type LineTest struct {
	start time.Time
	last  time.Duration
	rCtx  *R.Context
	verts []gl.Vec3
}

var _ meta.Animation = (*LineTest)(nil)

func (c *LineTest) Init(start image.Image) {
	c.start = time.Now()
	c.rCtx = R.New(start.Size())
	c.verts = make([]gl.Vec3, len(cube))
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
		gl.Vec3{
			(float32(math.Cos(t) * distance)),
			distance,
			(float32(math.Sin(t) * distance)),
		},
		gl.Vec3{0, 0, 0},
		gl.Vec3{0, 1, 0},
	)
	r.Clear()

	for i, vert := range cube {
		c.verts[i] = r.Transform(vert)
	}

	r.TriangleStrip(shader, c.verts)

	return current
}
