package colormap

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	L "github.com/cfoust/cy/pkg/layout"
	"github.com/cfoust/cy/pkg/layout/prop"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/sasha-s/go-deadlock"
)

type ColorMap struct {
	*L.Computable
	deadlock.RWMutex
	*mux.UpdatePublisher
	render *taro.Renderer
	screen mux.Screen
	size   geom.Size
	config L.ColorMapType
}

var _ mux.Screen = (*ColorMap)(nil)
var _ L.Reusable = (*ColorMap)(nil)

func (l *ColorMap) Apply(node L.NodeType) (bool, error) {
	config, ok := node.(L.ColorMapType)
	if !ok {
		return false, nil
	}

	l.Lock()
	defer l.Unlock()

	l.config = config

	layout := L.New(config.Node)
	for _, prop := range []prop.Presettable{
		config.Map,
	} {
		prop.SetLogger(l.Logger)
		prop.Preset(
			l.Ctx(),
			l.Context.Context(),
			&layout,
		)
		prop.ClearCache()
	}

	return true, nil
}

func (l *ColorMap) Kill() {
	l.RLock()
	screen := l.screen
	l.RUnlock()

	screen.Kill()
}

func (l *ColorMap) State() *tty.State {
	l.Lock()
	defer l.Unlock()

	var (
		size   = l.size
		config = l.config
	)

	innerState := l.screen.State()
	state := tty.New(size)
	tty.Copy(geom.Vec2{}, state, innerState)

	if value, ok := config.Map.GetPreset(); ok {
		value.Apply(state.Image)
	}

	return state
}

func (l *ColorMap) Send(msg mux.Msg) {
	l.RLock()
	l.RUnlock()
	l.screen.Send(msg)
}

func (l *ColorMap) Size() geom.Size {
	l.RLock()
	defer l.RUnlock()
	return l.size
}

func (l *ColorMap) poll(ctx context.Context) {
	updates := l.screen.Subscribe(ctx)

	for {
		select {
		case <-ctx.Done():
			return
		case event := <-updates.Recv():
			if _, ok := event.(L.NodeChangeEvent); ok {
				continue
			}
			l.Publish(event)
		}
	}
}

func (l *ColorMap) Resize(size geom.Size) error {
	l.Lock()
	l.size = size
	l.Unlock()
	return l.screen.Resize(size)
}

func New(ctx context.Context, screen mux.Screen) *ColorMap {
	c := L.NewComputable(ctx)
	colormap := &ColorMap{
		Computable:      c,
		UpdatePublisher: mux.NewPublisher(),
		size:            geom.DEFAULT_SIZE,
		screen:          screen,
		render:          taro.NewRenderer(),
	}

	go colormap.poll(colormap.Ctx())

	return colormap
}
