package slime

import (
	"math"
	"time"

	"github.com/cfoust/cy/pkg/anim/meta"
	"github.com/cfoust/cy/pkg/geom/image"
)

type Slime struct {
	frame     int
	zoomFrame int
	last      time.Duration
	sim       *Simulator
	start     image.Image
	current   image.Image
}

var _ meta.Animation = (*Slime)(nil)

func (f *Slime) Init(start image.Image) {
	f.start = start

	size := start.Size()
	f.sim = New(size.R, size.C)
}

func (f *Slime) Update(delta time.Duration) image.Image {
	if (delta - f.last) < (time.Second / 40) {
		return f.current
	}

	f.last = delta

	size := f.start.Size()
	cursor := Cursor{}

	// This determines how often we zoom
	zoom := math.Sin(float64(f.frame) / 80)
	if zoom > 0.3 {
		if f.zoomFrame == 0 {
			f.zoomFrame = f.frame
		}

		cursor.Pressed = true

		// Choose a location to zoom in on
		zoomX := math.Cos(float64(f.zoomFrame)/180) * float64(size.C/4)
		zoomY := math.Sin(float64(f.zoomFrame)/180) * float64(size.R/4)

		cursor.X = float64(size.C)/2 + zoomX
		cursor.Y = float64(size.R)/2 + zoomY
	} else {
		f.zoomFrame = 0
	}

	f.sim.Step(cursor)

	f.current = image.New(size)
	f.sim.Render(f.current)
	f.frame++
	return f.current
}
