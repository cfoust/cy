package frames

import (
	"math/rand"

	"github.com/cfoust/cy/pkg/geom/image"
)

type Frame func(image.Image)

var Frames = map[string]Frame{}

func registerFrame(name string, frame Frame) {
	Frames[name] = frame
}

func RandomFrame() Frame {
	chosen := rand.Int() % len(Frames)
	index := 0
	for _, frame := range Frames {
		if index == chosen {
			return frame
		}

		index++
	}

	return Hive
}
