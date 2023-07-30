package anim

import (
	"time"

	"github.com/cfoust/cy/pkg/geom/image"
)

type Animation interface {
	Init(image.Image)
	Update(time.Duration) image.Image
}
