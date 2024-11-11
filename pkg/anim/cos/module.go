package cos

import (
	"math"
	"time"

	"github.com/cfoust/cy/pkg/anim/meta"
	"github.com/cfoust/cy/pkg/geom/image"
)

type Cos struct {
	in  image.Image
	out image.Image
}

var _ meta.Animation = (*Cos)(nil)

func (cos *Cos) Init(start image.Image) {
	cos.in = start.Clone()
	cos.out = start
}

func (cos *Cos) Update(delta time.Duration) image.Image {
	elapsed := delta.Seconds()
	size := cos.out.Size()

	var factor float64
	var offset int
	for col := 0; col < size.C; col++ {
		factor = ((math.Cos((float64(col)/float64(size.C))*math.Pi*4+elapsed) + 1) / 8) * float64(size.R)
		offset = int(factor)
		for row := 0; row < size.R; row++ {
			cos.out[(row+offset)%size.R][col] = cos.in[row][col]
		}
	}

	return cos.out
}

func init() {
}
