package anim

import (
	"math"
	"time"

	"github.com/cfoust/cy/pkg/geom/image"
)

type Midjo struct {
	out image.Image
}

var _ Animation = (*Midjo)(nil)

func (m *Midjo) Init(start image.Image) {
	m.out = start
}

func (mid *Midjo) Update(delta time.Duration) image.Image {
	elapsed := delta.Seconds() / 10
	size := mid.out.Size()
	for row := 0; row < size.R; row++ {
		s := 1 - (2*float64(row))/float64(size.R)
		for col := 0; col < size.C; col++ {
			o := (2*float64(col))/float64(size.C) - 1
			d := math.Sqrt(o*o + s*s)
			l := (0.1 * elapsed) / math.Max(0.1, d)
			f := math.Sin(l)
			b := math.Cos(l)
			u := o*f - s*b
			m := int(math.Round(((o*b + s*f + 1) / 2) * float64(size.C)))
			h := int(math.Round(((u+1)/2)*float64(size.R))) % size.R
			g := ' '
			if !(m < 0 || m >= size.C || h < 0 || h >= size.R) {
				g = mid.out[h][m].Char
			}
			mid.out[row][col].Char = g
		}
	}
	return mid.out
}
