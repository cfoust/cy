package anim

import (
	"time"

	"github.com/cfoust/cy/pkg/fluid"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
)

type Fluid struct {
	sim   *fluid.Simulator
	start image.Image
}

var _ Animation = (*Fluid)(nil)

func (f *Fluid) Init(start image.Image) {
	f.start = start
	f.sim = fluid.New(100, 100, 100)
}

func fixIndex(value float64, max int) int {
	return geom.Clamp(int((value/100)*float64(max)), 0, max-1)
}

func (f *Fluid) Update(delta time.Duration) image.Image {
	size := f.start.Size()
	i := image.New(size)
	f.sim.Update(1.)
	for _, particle := range f.sim.Particles() {
		i[fixIndex(particle.Y, size.R)][fixIndex(particle.X, size.C)].Char = 'x'
	}
	return i
}

func init() {
	registerAnimation("fluid", func() Animation {
		return &Fluid{}
	})
}
