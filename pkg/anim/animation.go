package anim

import (
	"time"

	"github.com/cfoust/cy/pkg/geom/image"
)

type Animation interface {
	Init(image.Image)
	Update(delta time.Duration) image.Image
}

type Creator func() Animation

var Animations = map[string]Creator{}

func registerAnimation(name string, animation Creator) {
	Animations[name] = animation
}
