package anim

import (
	"time"

	"github.com/cfoust/cy/pkg/fluid"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
)

type Fluid struct {
	last    time.Duration
	sim     *fluid.Simulator
	start   image.Image
	current image.Image
}

var _ Animation = (*Fluid)(nil)

func (f *Fluid) Init(start image.Image) {
	f.start = start

	size := start.Size()

	var particles []fluid.Particle
	for row := 0; row < size.R; row++ {
		for col := 0; col < size.C; col++ {
			if start[row][col].IsEmpty() {
				continue
			}
			particles = append(particles, fluid.NewParticle(
				float64(col),
				float64(row),
				0,
				0,
			))
		}
	}

	f.sim = fluid.New(float64(size.C), float64(size.R), particles)
}

func safeClamp(value float64, max int) int {
	return geom.Clamp(int((value/100)*float64(max)), 0, max-1)
}

func (f *Fluid) Update(delta time.Duration) image.Image {
	if (delta - f.last) < (time.Second / TICKS_PER_SECOND) {
		return f.current
	}

	f.last = delta

	size := f.start.Size()
	f.current = image.New(size)
	f.sim.Update(1.)

	i := f.current
	particles := f.sim.Particles()

	var particle fluid.Particle
	var destRow, destCol int
	for row := 0; row < size.R; row++ {
		for col := 0; col < size.C; col++ {
			if f.start[row][col].IsEmpty() {
				continue
			}

			particle = particles[row*size.R+col]
			destRow = size.R - 1 - safeClamp(particle.Y, size.R)
			destCol = safeClamp(particle.X, size.C)
			i[destRow][destCol] = f.start[row][col]
		}
	}

	return i
}

func init() {
	registerAnimation("fluid", func() Animation {
		return &Fluid{}
	})
}
