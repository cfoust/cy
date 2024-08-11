package margins

import (
	"context"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/frames"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	L "github.com/cfoust/cy/pkg/layout"
	"github.com/cfoust/cy/pkg/layout/prop"
	"github.com/cfoust/cy/pkg/mux"
	S "github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/charmbracelet/lipgloss"

	"github.com/sasha-s/go-deadlock"
)

// Margins puts empty space around a Screen and centers it. In size mode,
// Margins attempts to keep the Screen at a fixed size (one or both dimensions
// fixed). In margin mode, the Screen is surrounded by fixed-size margins that
// do not change with the screen size.
type Margins struct {
	*L.Computable
	deadlock.RWMutex
	*mux.UpdatePublisher
	render *taro.Renderer

	screen mux.Screen

	// Whether Margins is in margin mode or size mode
	isMargins bool
	margins   geom.Size
	size      geom.Size

	outer  geom.Size
	inner  geom.Rect
	config L.MarginsType
}

var _ mux.Screen = (*Margins)(nil)
var _ L.Reusable = (*Margins)(nil)

func fitMargin(outer, margin int) int {
	if margin == 0 {
		return outer
	}

	return geom.Max(outer-(margin*2), 1)
}

func getSize(outer, desired int) int {
	if desired == 0 {
		return outer
	}

	if desired > outer {
		return outer
	}

	return desired
}

func (l *Margins) Apply(node L.NodeType) (bool, error) {
	config, ok := node.(L.MarginsType)
	if !ok {
		return false, nil
	}

	l.Lock()
	defer l.Unlock()

	l.config = config

	oldSize := l.size
	newSize := geom.Vec2{
		R: config.Rows,
		C: config.Cols,
	}

	if oldSize != newSize {
		l.setSize(newSize)
	}

	layout := L.New(config.Node)
	for _, prop := range []prop.Presettable{
		config.Border,
		config.BorderFg,
		config.BorderBg,
	} {
		prop.Preset(
			l.Ctx(),
			l.Context.Context(),
			&layout,
		)
		prop.SetLogger(l.Logger)
	}

	return true, l.recalculate()
}

func (l *Margins) Kill() {
	l.RLock()
	screen := l.screen
	l.RUnlock()

	screen.Kill()
}

func (l *Margins) State() *tty.State {
	l.RLock()
	var (
		inner  = l.inner
		outer  = l.outer
		config = l.config
	)
	l.RUnlock()

	innerState := l.screen.State()
	state := tty.New(outer)

	tty.Copy(inner.Position, state, innerState)

	size := state.Image.Size()
	for row := 0; row < size.R; row++ {
		for col := 0; col < size.C; col++ {
			if inner.Contains(geom.Vec2{
				R: row,
				C: col,
			}) {
				continue
			}
			state.Image[row][col].Mode |= emu.AttrTransparent
		}
	}

	borderStyle, ok := config.Border.GetPreset()
	if !ok || borderStyle.None() {
		return state
	}

	boxStyle := l.render.NewStyle().
		Border(borderStyle.Border).
		BorderForeground(lipgloss.Color("7")).
		BorderTop(true).
		BorderLeft(true).
		BorderRight(true).
		BorderBottom(true).
		Width(inner.Size.C).
		Height(inner.Size.R)

	if value, ok := config.BorderFg.GetPreset(); ok {
		boxStyle = boxStyle.BorderForeground(value.Color)
	}

	if value, ok := config.BorderBg.GetPreset(); ok {
		boxStyle = boxStyle.BorderBackground(value.Color)
	}

	l.render.RenderAt(
		state.Image,
		inner.Position.R-1,
		inner.Position.C-1,
		boxStyle.Render(""),
	)

	return state
}

func (l *Margins) Send(msg mux.Msg) {
	l.RLock()
	inner := l.inner
	l.RUnlock()
	l.screen.Send(taro.TranslateMouseMessage(
		msg,
		-inner.Position.C,
		-inner.Position.R,
	))
}

func (l *Margins) getInner(outer geom.Size) geom.Rect {
	// Resolve the desired margins to a real inner window size
	factor := geom.Size{
		R: fitMargin(outer.R, l.margins.R),
		C: fitMargin(outer.C, l.margins.C),
	}

	// Or just ignore it if there's a predetermined size
	if !l.isMargins {
		factor = l.size
	}

	inner := geom.Size{
		R: geom.Max(getSize(outer.R, factor.R), 1),
		C: geom.Max(getSize(outer.C, factor.C), 1),
	}

	if value, ok := l.config.Border.GetPreset(); ok && !value.None() {
		inner.C = geom.Max(
			inner.C-2,
			1,
		)
	}

	return geom.Rect{
		Position: outer.Center(inner),
		Size:     inner,
	}
}

func (l *Margins) setSize(size geom.Size) {
	l.isMargins = false
	l.size = geom.Size{
		R: geom.Max(0, size.R),
		C: geom.Max(0, size.C),
	}
}

func (l *Margins) Size() geom.Size {
	l.RLock()
	defer l.RUnlock()
	return l.size
}

func (l *Margins) poll(ctx context.Context) {
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

func (l *Margins) recalculate() error {
	outer := l.outer
	inner := l.getInner(outer)
	l.inner = inner
	return l.screen.Resize(inner.Size)
}

func (l *Margins) Resize(size geom.Size) error {
	l.Lock()
	l.outer = size
	defer l.Unlock()
	return l.recalculate()
}

func New(ctx context.Context, screen mux.Screen) *Margins {
	c := L.NewComputable(ctx)
	margins := &Margins{
		Computable:      c,
		UpdatePublisher: mux.NewPublisher(),
		size: geom.Size{
			C: 80,
		},
		screen: screen,
		render: taro.NewRenderer(),
	}

	go margins.poll(ctx)

	return margins
}

func Add(ctx context.Context, screen mux.Screen) mux.Screen {
	innerLayers := S.NewLayers()
	innerLayers.NewLayer(
		ctx,
		screen,
		S.PositionTop,
		S.WithOpaque,
		S.WithInteractive,
	)
	margins := New(ctx, innerLayers)

	outerLayers := S.NewLayers()
	frame := frames.NewFramer(ctx, frames.RandomFrame())
	outerLayers.NewLayer(
		ctx,
		frame,
		S.PositionTop,
	)

	outerLayers.NewLayer(
		ctx,
		margins,
		S.PositionTop,
		S.WithInteractive,
		S.WithOpaque,
	)

	return outerLayers
}
