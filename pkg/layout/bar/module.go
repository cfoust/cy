package bar

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	L "github.com/cfoust/cy/pkg/layout"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/sasha-s/go-deadlock"
)

type Bar struct {
	*L.Computable
	deadlock.RWMutex
	*mux.UpdatePublisher
	render     *taro.Renderer
	screen     mux.Screen
	size       geom.Size
	inner, bar geom.Rect
	config     L.BarType
}

var _ mux.Screen = (*Bar)(nil)
var _ L.Reusable = (*Bar)(nil)

func (t *Bar) Kill() {
	t.screen.Kill()
}

func (b *Bar) State() *tty.State {
	b.Lock()
	defer b.Unlock()

	var (
		size   = b.size
		inner  = b.inner
		bar    = b.bar
		screen = b.screen
		config = b.config
	)
	state := tty.New(size)

	var barState string
	layout := L.New(config.Node)
	if value, ok := config.Text.Get(
		b.Ctx(),
		b.Context.Context(),
		bar.Size,
		&layout,
	); ok {
		barState = value
	}

	barState = b.render.NewStyle().
		MaxWidth(bar.Size.C).
		MaxHeight(bar.Size.R).
		Render(barState)

	b.render.RenderAt(
		state.Image,
		bar.Position.R,
		bar.Position.C,
		barState,
	)

	screenState := screen.State()
	// We want to preserve transparency
	image.CopyRaw(inner.Position, state.Image, screenState.Image)

	tty.Copy(inner.Position, state, screenState)
	state.CursorVisible = screenState.CursorVisible
	if screenState.CursorVisible {
		cursor := screenState.Cursor
		cursor.C += inner.Position.C
		cursor.R += inner.Position.R
		state.Cursor = cursor
	}

	return state
}

func (b *Bar) Apply(node L.NodeType) (bool, error) {
	config, ok := node.(L.BarType)
	if !ok {
		return false, nil
	}

	b.Lock()
	defer b.Unlock()

	b.config = config
	config.Text.ClearCache()
	config.Text.SetLogger(b.Logger)

	return true, nil
}

func (b *Bar) Send(msg mux.Msg) {
	b.RLock()
	var (
		inner = b.inner
	)
	b.RUnlock()

	mouseMsg, ok := msg.(taro.MouseMsg)
	if !ok {
		b.screen.Send(msg)
		return
	}

	if !inner.Contains(mouseMsg.Vec2) {
		return
	}
	b.screen.Send(taro.TranslateMouseMessage(
		msg,
		-inner.Position.C,
		-inner.Position.R,
	))
}

func (b *Bar) Resize(size geom.Size) error {
	b.Lock()
	defer b.Unlock()

	b.size = size
	b.inner = geom.Rect{
		Size: geom.Vec2{
			R: geom.Max(0, size.R-1),
			C: size.C,
		},
	}
	b.bar = geom.Rect{
		Size: geom.Vec2{
			R: 1,
			C: size.C,
		},
	}

	if b.config.Bottom {
		b.bar.Position = geom.Vec2{
			R: geom.Max(0, size.R-1),
		}
	} else {
		b.inner.Position = geom.Vec2{R: 1}
	}

	b.config.Text.ClearCache()

	return b.screen.Resize(b.inner.Size)
}

func (b *Bar) poll(ctx context.Context) {
	updates := b.screen.Subscribe(ctx)

	for {
		select {
		case <-ctx.Done():
			return
		case event := <-updates.Recv():
			if _, ok := event.(L.NodeChangeEvent); ok {
				continue
			}
			b.Publish(event)
		}
	}
}

func New(
	ctx context.Context,
	screen mux.Screen,
) *Bar {
	c := L.NewComputable(ctx)
	bar := &Bar{
		Computable:      c,
		UpdatePublisher: mux.NewPublisher(),
		screen:          screen,
		size:            geom.DEFAULT_SIZE,
		render:          taro.NewRenderer(),
	}

	go bar.poll(bar.Ctx())

	return bar
}
