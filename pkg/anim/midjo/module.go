package midjo

import (
	"math"
	"math/rand"
	"time"

	"github.com/cfoust/cy/pkg/anim/meta"
	"github.com/cfoust/cy/pkg/geom/image"
	R "github.com/cfoust/cy/pkg/rasterion"
	gl "github.com/go-gl/mathgl/mgl32"
)

type Midjo struct {
	in  image.Image
	out image.Image

	center gl.Vec2
}

var _ meta.Animation = (*Midjo)(nil)

func (m *Midjo) Init(start image.Image) {
	m.in = start.Clone()
	m.out = start

	r := rand.New(rand.NewSource(int64(rand.Int())))
	m.center[0] = (r.Float32() * 2) - 1
	m.center[1] = (r.Float32() * 2) - 1
}

// toDevice transforms a vec2 where x and y are in the range [0, 1] (left ->
// right, up -> down) to one in the range [-1, 1] (left -> right, down -> up)
func toDevice(v *gl.Vec2) {
	v[0] = 2*v.X() - 1
	v[1] = 1 - 2*v.Y()
}

// fromDevice performs the inverse of toDevice.
func fromDevice(v *gl.Vec2) {
	v[0] = (v.X() + 1) / 2
	v[1] = (v.Y() + 1) / 2
}

func (mid *Midjo) Update(delta time.Duration) image.Image {
	var (
		elapsed = delta.Seconds()
		size    = mid.out.Size()
		v       gl.Vec2
	)

	for row := 0; row < size.R; row++ {
		for col := 0; col < size.C; col++ {
			v[0] = float32(col) / float32(size.C)
			v[1] = float32(row) / float32(size.R)

			toDevice(&v)

			var (
				l = ((0.1 * elapsed) / math.Max(0.1, float64(v.Len())))
				f = float32(math.Sin(l))
				b = float32(math.Cos(l))
				u = v.X()*f - v.Y()*b
			)
			v[0] = v.X()*b + v.Y()*f
			v[1] = u
			fromDevice(&v)
			mid.out[row][col] = R.Sample2D(mid.in, v)
		}
	}

	return mid.out
}
