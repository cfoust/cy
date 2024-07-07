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

const (
	POSITION_FACTOR = 10
)

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
				float64(col)*POSITION_FACTOR,
				float64(size.R-1-row)*POSITION_FACTOR,
				0,
				0,
			))
		}
	}

	f.sim = fluid.New(
		float64(size.C)*POSITION_FACTOR,
		float64(size.R)*POSITION_FACTOR,
		particles,
	)
}

func safeClamp(value, bound float64, max int) int {
	return geom.Clamp(int((value/bound)*float64(max)), 0, max-1)
}

func (f *Fluid) Update(delta time.Duration) image.Image {
	if (delta - f.last) < (time.Second / TICKS_PER_SECOND) {
		return f.current
	}

	f.last = delta

	size := f.start.Size()
	f.current = image.New(size)
	f.sim.Update(1)

	i := f.current
	particles := f.sim.Particles()

	var particle fluid.Particle
	var index, destRow, destCol int
	for row := 0; row < size.R; row++ {
		for col := 0; col < size.C; col++ {
			if f.start[row][col].IsEmpty() {
				continue
			}

			if index >= len(particles) {
				continue
			}

			particle = particles[index]
			destRow = safeClamp(
				particle.Y/POSITION_FACTOR,
				float64(size.R),
				size.R,
			)
			destCol = safeClamp(
				particle.X/POSITION_FACTOR,
				float64(size.C),
				size.C,
			)
			i[destRow][destCol] = f.start[row][col]
			index++
		}
	}

	return i
}

func init() {
	registerAnimation("fluid", func() Animation {
		return &Fluid{}
	})
}
