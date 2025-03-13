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
	distance         = 0.1
)

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

var screenVerts = []gl.Vec3{
	{-1, 1, -1}, // Back-top-left
	{1, 1, -1},  // Back-top-right
	{-1, 1, 1},  // Front-top-left
	{1, 1, 1},   // Front-top-right
}

var screenUvs = []gl.Vec2{
	{0, 0},
	{1.0, 0},
	{0.0, 1.0},
	{1.0, 1.0},
}

var screenFaces = [][3]int{
	{2, 1, 0},
	{1, 2, 3},
}

type topShader struct {
	R.TransformShader
	texture image.Image
}

var _ R.Shader = (*topShader)(nil)

func (s *topShader) Fragment(
	i0, i1, i2 int,
	bary gl.Vec3,
) (glyph emu.Glyph, discard bool) {
	m := gl.Mat2x3FromCols(
		screenUvs[i0],
		screenUvs[i1],
		screenUvs[i2],
	)
	uv := m.Mul3x1(bary)
	glyph = R.Texture(s.texture, uv)
	return
}

type LineTest struct {
	start time.Time
	last  time.Duration
	rCtx  *R.Context
	top   *topShader
}

var _ meta.Animation = (*LineTest)(nil)

func (c *LineTest) Init(start image.Image) {
	c.start = time.Now()
	r := R.New(start.Size())
	c.rCtx = r

	camera := r.Camera()
	camera.View = gl.LookAtV(
		gl.Vec3{0, 5., 0},
		gl.Vec3{0, 0, 0},
		gl.Vec3{0, 0, -1.},
	)

	size := start.Size()
	screenPoint := gl.Vec2{
		float32(size.C),
		float32(size.R),
	}
	a, _ := camera.UnProject(screenPoint.Vec3(1.0))
	b, _ := camera.UnProject(screenPoint.Vec3(0.9))
	p, _ := LinePlaneIntersection(
		gl.Vec3{0, 1, 0},
		gl.Vec3{0, 1, 0},
		a,
		b.Sub(a),
	)

	c.top = &topShader{
		texture: start,
	}
	c.top.M = gl.Scale3D(
		float32(math.Abs(float64(p[0]))),
		1.0,
		float32(math.Abs(float64(p[2]))),
	)
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
	camera := r.Camera()

	camera.View = gl.LookAtV(
		gl.Vec3{
			(float32(math.Sin(t) * d)),
			5.,
			(float32(math.Cos(t) * d)),
		},
		gl.Vec3{0, 0, 0},
		gl.Vec3{0, 1, 0},
	)
	r.Clear()

	r.Triangles(c.top, screenVerts, screenFaces)

	return current
}
