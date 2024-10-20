package anim

import (
	"math"
	"time"

	"github.com/cfoust/cy/pkg/anim/slime"
	"github.com/cfoust/cy/pkg/geom/image"
)

type Slime struct {
	frame   int
	last    time.Duration
	sim     *slime.Simulator
	start   image.Image
	current image.Image
}

var _ Animation = (*Slime)(nil)

func (f *Slime) Init(start image.Image) {
	f.start = start

	size := start.Size()
	f.sim = slime.New(size.R, size.C)
}

func (f *Slime) Update(delta time.Duration) image.Image {
	if (delta - f.last) < (time.Second / 40) {
		return f.current
	}

	f.last = delta

	size := f.start.Size()
	cursor := slime.Cursor{}
	if math.Sin(float64(f.frame)/50) > 0.6 {
		cursor.Pressed = true
		cursor.X = float64(size.C) / 2
		cursor.Y = float64(size.R) / 2
	}

	f.sim.Step(cursor)

	f.current = image.New(size)
	f.sim.Render(f.current)
	f.frame++
	return f.current
}

func init() {
	registerAnimation("slime", func() Animation {
		return &Slime{}
	})
}
