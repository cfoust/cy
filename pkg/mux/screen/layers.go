package screen

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/util"

	"github.com/sasha-s/go-deadlock"
)

type Layer struct {
	util.Lifetime
	Screen
	isInteractive bool
}

// Layers allows you to stack several Screens on top of one another. The
// topmost layer with isInteractive set to true receives all key presses.
type Layers struct {
	deadlock.RWMutex
	size    geom.Vec2
	layers  []*Layer
	changes *mux.UpdatePublisher
}

var _ Screen = (*Layers)(nil)

func (l *Layers) State() *tty.State {
	l.RLock()
	defer l.RUnlock()

	state := tty.New(l.size)

	for _, layer := range l.layers {
		image.Compose(state.Image, 0, 0, layer.State().Image)
	}

	return state
}

func (l *Layers) Updates() *Updater {
	return l.changes.Subscribe()
}

func (l *Layers) NewLayer(ctx context.Context, screen Screen, isInteractive bool) *Layer {
	l.Lock()
	defer l.Unlock()

	layer := &Layer{
		Lifetime:      util.NewLifetime(ctx),
		Screen:        screen,
		isInteractive: isInteractive,
	}

	l.layers = append(l.layers, layer)

	go func() {
		<-layer.Ctx().Done()

		l.Lock()
		newLayers := make([]*Layer, 0)
		for _, other := range l.layers {
			if layer == other {
				continue
			}

			newLayers = append(newLayers, other)
		}
		l.layers = newLayers
		l.Unlock()
	}()

	return layer
}

func (l *Layers) Write(data []byte) (n int, err error) {
	l.RLock()
	defer l.RUnlock()

	layers := l.layers

	for i := len(layers) - 1; i >= 0; i-- {
		layer := layers[i]
		if !layer.isInteractive {
			continue
		}

		return layer.Write(data)
	}

	return 0, nil
}

func (l *Layers) Resize(size Size) error {
	l.Lock()
	l.size = size
	l.Unlock()

	for _, layer := range l.layers {
		layer.Resize(size)
	}

	return nil
}

func NewLayers() *Layers {
	return &Layers{
		changes: mux.NewPublisher(),
		size:    geom.DEFAULT_SIZE,
	}
}
