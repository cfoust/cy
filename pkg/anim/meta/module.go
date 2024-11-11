package meta

import (
	"time"

	"github.com/cfoust/cy/pkg/geom/image"
)

type Animation interface {
	Init(image.Image)
	Update(delta time.Duration) image.Image
}

type Creator func() Animation

