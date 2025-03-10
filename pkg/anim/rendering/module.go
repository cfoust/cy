package rendering

import (
	"math"
	"time"

	"github.com/cfoust/cy/pkg/anim/meta"
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom/image"
	R "github.com/cfoust/cy/pkg/rasterion"

	gl "github.com/go-gl/mathgl/mgl32"
	"github.com/rs/zerolog/log"
)

const (
	TICKS_PER_SECOND = 30
	distance         = 0.1
)

// https://stackoverflow.com/a/46016469
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

func LinePlaneIntersection(
	planePoint,
	planeNormal,
	linePoint,
	lineDir gl.Vec3,
) (gl.Vec3, bool) {
	denom := planeNormal.Dot(lineDir)

	// Check if the line is parallel to the plane
	if denom == 0 {
		// No intersection or line lies in the plane
		return gl.Vec3{}, false
	}

	// Compute the intersection parameter t
	t := planeNormal.Dot(planePoint.Sub(linePoint)) / denom

	// Compute the intersection point
	intersection := linePoint.Add(lineDir.Mul(t))
	return intersection, true
}

type LineTest struct {
	start time.Time
	last  time.Duration
	rCtx  *R.Context
	verts []gl.Vec3
	model gl.Mat4
}

var _ meta.Animation = (*LineTest)(nil)

func (c *LineTest) Init(start image.Image) {
	c.start = time.Now()
	r := R.New(start.Size())
	c.rCtx = r
	c.verts = make([]gl.Vec3, len(cube))

	r.Camera.View = gl.LookAtV(
		gl.Vec3{0, 5., 0},
		gl.Vec3{0, 0, 0},
		gl.Vec3{0, 0, -1.},
	)

	size := start.Size()
	screenPoint := gl.Vec2{
		float32(size.C),
		float32(size.R),
	}
	a, _ := r.UnProject(screenPoint.Vec3(1.0))
	b, _ := r.UnProject(screenPoint.Vec3(0.9))
	p, ok := LinePlaneIntersection(
		gl.Vec3{0, 1, 0},
		gl.Vec3{0, 1, 0},
		a,
		b.Sub(a),
	)
	log.Info().Msgf("%+v %+v", p, ok)

	c.model = gl.Scale3D(
		float32(math.Abs(float64(p[0]))),
		0.5,
		float32(math.Abs(float64(p[2]))),
	)
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

func lerp(t, a, b float64) float64 {
	return a + t*(b-a)
}

func (c *LineTest) Update(delta time.Duration) image.Image {
	current := c.rCtx.Image()
	if (delta - c.last) < (time.Second / TICKS_PER_SECOND) {
		return current
	}

	c.last = delta

	t := time.Now().Sub(c.start).Seconds() / 3

	r := c.rCtx
	d := math.Min(lerp(t/5.0, 0, 5.0), 5.0)
	r.Camera.View = gl.LookAtV(
		gl.Vec3{
			(float32(math.Sin(t) * d)),
			5.,
			(float32(math.Cos(t) * d)),
		},
		gl.Vec3{0, 0, 0},
		gl.Vec3{0, 1, 0},
	)
	r.Clear()

	for i, vert := range cube {
		vert = c.model.Mul4x1(vert.Vec4(1)).Vec3()
		vert = gl.Translate3D(0, 1, 0).Mul4x1(vert.Vec4(1)).Vec3()
		c.verts[i] = r.Project(vert)
	}

	r.TriangleStrip(shader, c.verts)

	return current
}
