package city

import (
	"math"
	"time"

	"github.com/cfoust/cy/pkg/anim/meta"
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

type City struct {
	start    time.Time
	last     time.Duration
	rCtx     *R.Context
	screen   *screenShader
	building *buildingShader
}

var _ meta.Animation = (*City)(nil)

func (c *City) Init(start image.Image) {
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
		gl.Vec3{0, 0, 0},
		gl.Vec3{0, 1, 0},
		a,
		b.Sub(a),
	)

	c.screen = &screenShader{
		texture: start,
	}
	c.screen.M = gl.Scale3D(
		float32(math.Abs(float64(p[0]))),
		1.0,
		float32(math.Abs(float64(p[2]))),
	)

	c.building = &buildingShader{}
}

func lerp(t, a, b float64) float64 {
	return a + t*(b-a)
}

func (c *City) Update(delta time.Duration) image.Image {
	current := c.rCtx.Image()
	if (delta - c.last) < (time.Second / TICKS_PER_SECOND) {
		return current
	}

	c.last = delta

	t := time.Now().Sub(c.start).Seconds() / 3
	t += 5
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

	c.screen.Draw(r)
	c.building.Size[0] = 0.25
	c.building.Size[1] = 1
	c.building.Position[0] = 3
	c.building.Draw(r)

	return current
}
