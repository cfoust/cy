package frames

import (
	"math/rand"
	"time"

	"github.com/cfoust/cy/pkg/geom/image"
)

type Animation interface {
	Init(image.Image)
	Update(delta time.Duration) image.Image
}

func RandomAnimation() Animation {
	anims := []Animation{
		&Collapse{},
		&Midjo{},
		&Cyform{},
		&Conway{},
	}

	return anims[rand.Int()%len(anims)]
}

var Animations = map[string]Animation{}

func registerAnimation(name string, animation Animation) {
	Animations[name] = animation
}
