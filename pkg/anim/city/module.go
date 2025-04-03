package city

import (
	"math"
	"math/rand"
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

	buildingSize float32
	screenBounds R.Rect

	seed int64
	rand *rand.Rand
}

var _ meta.Animation = (*City)(nil)

const (
	buildingSize = 0.5
	buildingGap  = 0.4
	gridSize     = 16.
	halfGridSize = gridSize / 2
)

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
	// Point in screen space we want to sample at
	screenPoint := gl.Vec2{
		float32(size.C),
		float32(size.R),
	}

	// Get two points a, b that differ only in depth to create a line
	// pointing at ground
	a, _ := camera.UnProject(screenPoint.Vec3(1.0))
	b, _ := camera.UnProject(screenPoint.Vec3(0.9))

	// Find the point on the ground plane
	p, _ := LinePlaneIntersection(
		gl.Vec3{0, 0, 0},
		gl.Vec3{0, 1, 0},
		a,
		b.Sub(a),
	)

	// Half the size of the screen in world space
	var (
		screenWidth  = float32(math.Abs(float64(p[0])))
		screenHeight = float32(math.Abs(float64(p[2])))
	)
	c.screen = &screenShader{
		texture: start,
	}
	screenMatrix := gl.Scale3D(screenWidth, 1.0, screenHeight)
	c.screen.M = screenMatrix

	topLeft := screenMatrix.
		Mul4x1(screenVerts[0].Vec4(1)).
		Vec3()

	c.screenBounds = R.Rect{
		Pos:  gl.Vec2{topLeft[0], topLeft[2]},
		Size: gl.Vec2{screenWidth * 2, screenHeight * 2},
	}

	c.building = &buildingShader{
		Size: gl.Vec2{buildingSize, 0.5},
	}

	c.seed = int64(rand.Int())
	c.rand = rand.New(rand.NewSource(c.seed))
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

	var (
		origin = gl.Vec3{
			-1 * halfGridSize,
			0,
			-1 * halfGridSize,
		}
		gridRows = float32(gridSize / (buildingSize + buildingGap))
	)

	c.rand.Seed(c.seed)
	var pos gl.Vec3
	var bounds = R.Rect{
		Size: gl.Vec2{buildingSize, buildingSize},
	}
	for row := 0; row < int(gridRows); row++ {
		for col := 0; col < int(gridRows); col++ {
			pos = origin.Add(gl.Vec3{
				float32(row) * (buildingSize + buildingGap),
				0,
				float32(col) * (buildingSize + buildingGap),
			})

			bounds.Pos = gl.Vec2{pos[0], pos[2]}
			if c.screenBounds.Overlaps(bounds) {
				continue
			}

			c.building.Type = c.rand.Intn(6)
			c.building.Position = pos
			c.building.Draw(r)
		}
	}

	return current
}
