package anim

import (
	"math"
	"math/rand"
	"time"

	"github.com/cfoust/cy/pkg/anim/slime"
	"github.com/cfoust/cy/pkg/geom/image"
)

type Slime struct {
	last    time.Duration
	sim     *slime.Simulator
	start   image.Image
	current image.Image
}

var _ Animation = (*Slime)(nil)

func (f *Slime) Init(start image.Image) {
	f.start = start

	size := start.Size()

	var agents []slime.Agent
	for row := 0; row < size.R; row++ {
		for col := 0; col < size.C; col++ {
			if start[row][col].IsEmpty() {
				continue
			}
			agents = append(agents, slime.Agent{
				Pos: slime.Vec2{
					X: float64(col),
					Y: float64(row),
				},
				Dir: slime.Vec2{X: 1, Y: 0}.
					Rot(rand.Float64() * 2 * math.Pi),
			})
		}
	}

	f.sim = slime.New(
		size.R,
		size.C,
		agents,
	)
}

func (f *Slime) Update(delta time.Duration) image.Image {
	if (delta - f.last) < (time.Second / 40) {
		return f.current
	}

	f.last = delta

	f.sim.Step(slime.Cursor{})

	size := f.start.Size()
	f.current = image.New(size)
	f.sim.Render(f.current)
	return f.current
}

func init() {
	registerAnimation("slime", func() Animation {
		return &Slime{}
	})
}
