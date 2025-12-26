package maelstrom

import (
	"math"
	"math/rand"
	"time"

	"github.com/cfoust/cy/pkg/anim/meta"
	"github.com/cfoust/cy/pkg/geom/image"
	R "github.com/cfoust/cy/pkg/rasterion"

	gl "github.com/go-gl/mathgl/mgl32"
)

type Maelstrom struct {
	in  image.Image
	out image.Image

	center gl.Vec2
}

var _ meta.Animation = (*Maelstrom)(nil)

func (m *Maelstrom) Init(start image.Image) {
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
	v[1] = 1 - ((v.Y() + 1) / 2)
}

func (m *Maelstrom) Update(delta time.Duration) image.Image {
	var (
		elapsed = delta.Seconds()
		size    = m.out.Size()
		v       gl.Vec2
		angle   float32
	)

	for row := 0; row < size.R; row++ {
		for col := 0; col < size.C; col++ {
			v[0] = float32(col) / float32(size.C)
			v[1] = float32(row) / float32(size.R)
			toDevice(&v)
			v = v.Sub(m.center)
			angle = float32((0.1 * elapsed) / math.Max(0.1, float64(v.Len())))
			v = gl.Rotate2D(angle).Mul2x1(v)
			v = v.Add(m.center)
			fromDevice(&v)
			m.out[row][col] = R.Sample2D(m.in, v)
		}
	}

	return m.out
}
