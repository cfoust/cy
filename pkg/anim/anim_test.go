package anim

import (
	"testing"
	"time"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
)

// Subject all animations to a few simple smoke tests.
func TestAnims(t *testing.T) {
	for name, anim := range Animations {
		t.Logf("testing anim %s", name)

		// The most common point of failure for animations is failing
		// to do adequate bounds checks. This renders every animation
		// at a few different sizes to ensure that doesn't happen.
		for _, size := range []geom.Size{
			geom.Size{},
			geom.Size{
				R: 5,
				C: 5,
			},
			geom.DEFAULT_SIZE,
		} {
			instance := anim()
			instance.Init(image.New(size))
			instance.Update(0)
			instance.Update(time.Second)
		}
	}
}
