package tabs

import (
	"context"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	L "github.com/cfoust/cy/pkg/layout"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/charmbracelet/lipgloss"
	"github.com/sasha-s/go-deadlock"
)

type Tabs struct {
	deadlock.RWMutex
	*mux.UpdatePublisher
	render     *taro.Renderer
	screen     mux.Screen
	size       geom.Size
	inner, bar geom.Rect
	config     L.TabsType
}

var _ mux.Screen = (*Tabs)(nil)
var _ L.Reusable = (*Tabs)(nil)

func (t *Tabs) Kill() {
	t.screen.Kill()
}

func (t *Tabs) State() *tty.State {
	t.RLock()
	var (
		size   = t.size
		inner  = t.inner
		bar    = t.bar
		screen = t.screen
		config = t.config
	)
	t.RUnlock()
	state := tty.New(size)

	activeFg := lipgloss.Color("0")
	if config.ActiveFg != nil {
		activeFg = config.ActiveFg.Color
	}

	activeBg := lipgloss.Color("7")
	if config.ActiveBg != nil {
		activeFg = config.ActiveBg.Color
	}

	tabStyle := t.render.NewStyle().
		Padding(0, 1)

	active := tabStyle.
		Foreground(activeFg).
		Background(activeBg)

	inactiveFg := lipgloss.Color("7")
	if config.InactiveFg != nil {
		inactiveFg = config.InactiveFg.Color
	}

	inactiveBg := lipgloss.Color("0")
	if config.InactiveBg != nil {
		inactiveBg = config.InactiveBg.Color
	}

	inactive := tabStyle.
		Foreground(inactiveFg).
		Background(inactiveBg)

	bg := emu.DefaultBG
	if config.Bg != nil {
		bg = config.Bg.Emu()
	}

	var tabs []image.Image
	for _, tab := range config.Tabs {
		name := tab.Name
		cols := lipgloss.Width(name)

		// If the given name contains ANSI escape sequences, we don't
		// use the provided fg/bg colors and just render the name
		// directly.
		if len(name) == cols {
			if tab.Active {
				name = active.Render(tab.Name)
			} else {
				name = inactive.Render(tab.Name)
			}
			cols = lipgloss.Width(name)
		}

		i := image.New(geom.Vec2{
			R: 1,
			C: cols,
		})
		t.render.RenderAt(
			i,
			0, 0,
			name,
		)
		tabs = append(tabs, i)
	}

	for col := 0; col < size.C; col++ {
		state.Image[bar.Position.R][col].BG = bg
	}

	var col int
	for _, tab := range tabs {
		image.Copy(
			geom.Vec2{
				C: col,
				R: bar.Position.R,
			},
			state.Image,
			tab,
		)
		col += tab.Size().C
	}

	tty.Copy(inner.Position, state, screen.State())

	return state
}

func (t *Tabs) Apply(node L.NodeType) (bool, error) {
	config, ok := node.(L.TabsType)
	if !ok {
		return false, nil
	}

	t.Lock()
	defer t.Unlock()

	t.config = config
	return true, nil
}

func (t *Tabs) Send(msg mux.Msg) {
	t.screen.Send(msg)
}

func (t *Tabs) Resize(size geom.Size) error {
	t.Lock()
	defer t.Unlock()

	t.size = size
	t.inner = geom.Rect{
		Size: geom.Vec2{
			R: geom.Max(0, size.R-1),
			C: size.C,
		},
	}
	t.bar = geom.Rect{
		Size: geom.Vec2{
			R: 1,
			C: size.C,
		},
	}

	if t.config.Bottom {
		t.bar.Position = geom.Vec2{
			R: geom.Max(0, size.R-1),
		}
	} else {
		t.inner.Position = geom.Vec2{R: 1}
	}

	return t.screen.Resize(t.inner.Size)
}

func (t *Tabs) poll(ctx context.Context) {
	updates := t.screen.Subscribe(ctx)

	for {
		select {
		case <-ctx.Done():
			return
		case event := <-updates.Recv():
			if _, ok := event.(L.NodeChangeEvent); ok {
				continue
			}
			t.Publish(event)
		}
	}
}

func New(ctx context.Context, screen mux.Screen) *Tabs {
	tabs := &Tabs{
		UpdatePublisher: mux.NewPublisher(),
		screen:          screen,
		size:            geom.DEFAULT_SIZE,
		render:          taro.NewRenderer(),
	}

	go tabs.poll(ctx)

	return tabs
}
