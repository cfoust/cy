package anim

import (
	"math/rand"
	"time"

	"github.com/cfoust/cy/pkg/geom/image"
)

type Animation interface {
	Init(image.Image)
	Update(delta time.Duration) image.Image
}

type Creator func() Animation

func RandomAnimation() Animation {
	anims := []Animation{
		&Fluid{},
		&Midjo{},
		&Cyform{},
		&Conway{},
		&Cos{},
	}

	return anims[rand.Int()%len(anims)]
}

var Animations = map[string]Creator{}

func registerAnimation(name string, animation Creator) {
	Animations[name] = animation
}
