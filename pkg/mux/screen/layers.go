package screen

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"

	"github.com/sasha-s/go-deadlock"
)

type Layer struct {
	Screen
	isInteractive bool
	isOpaque      bool
}

// Layers allows you to stack several Screens on top of one another. The
// topmost layer with isInteractive set to true receives all key presses.
type Layers struct {
	deadlock.RWMutex
	*mux.UpdatePublisher
	size   geom.Vec2
	layers []*Layer
}

var _ Screen = (*Layers)(nil)

type RenderLayer struct {
	state *tty.State
	layer *Layer
}

func (l *Layers) State() *tty.State {
	l.RLock()
	defer l.RUnlock()

	state := tty.New(l.size)

	states := make([]RenderLayer, 0)

	// We don't want to invoke State() separately in two different passes
	for _, layer := range l.layers {
		states = append(states, RenderLayer{
			state: layer.State(),
			layer: layer,
		})
	}

	// In the first pass we layer the states on top of each other
	for _, layer := range states {
		if layer.layer.isOpaque {
			image.Copy(geom.Vec2{}, state.Image, layer.state.Image)
		} else {
			image.Compose(geom.Vec2{}, state.Image, layer.state.Image)
		}
	}

	// Then we find the topmost cursor state for interactivity
	foundCursor := false
	for i := len(states) - 1; i >= 0; i-- {
		layer := states[i]
		if !foundCursor && layer.layer.isInteractive {
			foundCursor = true
			state.Cursor = layer.state.Cursor
			state.CursorVisible = layer.state.CursorVisible
		}
	}

	if !foundCursor {
		state.CursorVisible = false
	}

	return state
}

func (l *Layers) rerender() {
	l.Notify()
}

func (l *Layers) NumLayers() int {
	l.RLock()
	defer l.RUnlock()
	return len(l.layers)
}

type LayerOption func(*Layer)

func WithInteractive(layer *Layer) {
	layer.isInteractive = true
}

func WithOpaque(layer *Layer) {
	layer.isOpaque = true
}

type Position int

const (
	PositionTop Position = iota
	PositionBottom
)

func (l *Layers) NewLayer(ctx context.Context, screen Screen, pos Position, options ...LayerOption) *Layer {
	layer := &Layer{
		Screen: screen,
	}

	for _, option := range options {
		option(layer)
	}

	l.Lock()
	if pos == PositionTop {
		l.layers = append(l.layers, layer)
	} else {
		l.layers = append([]*Layer{layer}, l.layers...)
	}
	l.Unlock()

	go func() {
		updates := layer.Subscribe(ctx)
		for {
			select {
			case event := <-updates.Recv():
				l.Publish(event)
			case <-ctx.Done():
				return
			}
		}
	}()

	go func() {
		<-ctx.Done()

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

		l.rerender()
	}()

	layer.Resize(l.size)

	return layer
}

func (l *Layers) Send(msg mux.Msg) {
	l.RLock()
	layers := l.layers
	l.RUnlock()

	for i := len(layers) - 1; i >= 0; i-- {
		layer := layers[i]
		if !layer.isInteractive {
			continue
		}

		layer.Send(msg)
		return
	}
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
		UpdatePublisher: mux.NewPublisher(),
		size:            geom.DEFAULT_SIZE,
	}
}
