package tabs

import (
	"context"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	L "github.com/cfoust/cy/pkg/layout"
	"github.com/cfoust/cy/pkg/layout/prop"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/charmbracelet/lipgloss"
	"github.com/sasha-s/go-deadlock"
)

type Tabs struct {
	*L.Computable
	deadlock.RWMutex
	*mux.UpdatePublisher
	render     *taro.Renderer
	screen     mux.Screen
	size       geom.Size
	inner, bar geom.Rect
	config     L.TabsType

	// The tab bar at the time of the last render. Saving this lets us use
	// it for hit detection on clicks.
	lastBar image.Image
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
	if value, ok := config.ActiveFg.GetPreset(); ok {
		activeFg = value.Color
	}

	activeBg := lipgloss.Color("4")
	if value, ok := config.ActiveBg.GetPreset(); ok {
		activeBg = value.Color
	}

	tabStyle := t.render.NewStyle().
		Padding(0, 1)

	active := tabStyle.
		Foreground(activeFg).
		Background(activeBg)

	inactiveFg := lipgloss.Color("0")
	if value, ok := config.InactiveBg.GetPreset(); ok {
		inactiveFg = value.Color
	}

	inactiveBg := lipgloss.Color("7")
	if value, ok := config.InactiveBg.GetPreset(); ok {
		inactiveBg = value.Color
	}

	inactive := tabStyle.
		Foreground(inactiveFg).
		Background(inactiveBg)

	bg := emu.DefaultBG
	if value, ok := config.Bg.GetPreset(); ok {
		bg = value.Emu()
	}

	var barWidth, activeLoc, activeWidth int
	var tabs []image.Image
	for index, tab := range config.Tabs {
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

		// Save the tab index somewhere for click hitscan
		for col := 0; col < cols; col++ {
			i[0][col].Write = emu.WriteID(index)
		}

		tabs = append(tabs, i)

		if tab.Active {
			activeLoc = barWidth
			activeWidth = cols
		}

		barWidth += cols
	}

	for col := 0; col < size.C; col++ {
		state.Image[bar.Position.R][col].BG = bg
	}

	// Render the tab bar into one long line so we can shift it
	// appropriately when the active tab is not on the screen
	renderedBar := image.New(geom.Vec2{
		R: 1,
		C: barWidth,
	})
	var col int
	for _, tab := range tabs {
		image.Copy(geom.Vec2{C: col}, renderedBar, tab)
		col += tab.Size().C
	}

	barOffset := geom.Clamp(
		activeLoc+(activeWidth/2)-(size.C/2),
		0,
		geom.Max(0, barWidth-size.C),
	)
	renderedBar[0] = renderedBar[0][barOffset:]
	image.Copy(bar.Position, state.Image, renderedBar)

	// Save this for hit detection
	t.Lock()
	t.lastBar = renderedBar
	t.Unlock()

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

func (t *Tabs) Apply(node L.NodeType) (bool, error) {
	config, ok := node.(L.TabsType)
	if !ok {
		return false, nil
	}

	t.Lock()
	defer t.Unlock()

	layout := L.New(config.Active().Node)
	for _, prop := range []*prop.Color{
		config.ActiveFg,
		config.ActiveBg,
		config.InactiveFg,
		config.InactiveBg,
		config.Bg,
	} {
		prop.Preset(
			t.Ctx(),
			t.Context.Context(),
			&layout,
		)
		prop.SetLogger(t.Logger)
	}

	t.config = config
	return true, nil
}

func (t *Tabs) Send(msg mux.Msg) {
	t.RLock()
	var (
		lastBar = t.lastBar
		inner   = t.inner
		bar     = t.bar
	)
	t.RUnlock()

	mouseMsg, ok := msg.(taro.MouseMsg)
	if !ok {
		t.screen.Send(msg)
		return
	}

	if inner.Contains(mouseMsg.Vec2) {
		t.screen.Send(taro.TranslateMouseMessage(
			msg,
			-inner.Position.C,
			-inner.Position.R,
		))
		return
	}

	// Must be a click
	if mouseMsg.Type != taro.MousePress || mouseMsg.Button != taro.MouseLeft || mouseMsg.Down {
		return
	}

	// And must be inside the bounds of the bar
	if !bar.Contains(mouseMsg.Vec2) || mouseMsg.C < 0 || mouseMsg.C >= lastBar.Size().C {
		return
	}

	tabIndex := int(lastBar[0][mouseMsg.C].Write)
	t.RLock()
	var (
		config = L.Copy(t.config).(L.TabsType)
	)
	t.RUnlock()

	if tabIndex == config.ActiveIndex() {
		return
	}

	isAttached := L.IsAttached(config)

	if isAttached {
		if newConfig, ok := L.Detach(config).(L.TabsType); ok {
			config = newConfig
		}
	}

	var newTabs []L.Tab
	for index, tab := range config.Tabs {
		// Do nothing if we're already on this tab
		if index == tabIndex && tab.Active {
			return
		}

		isActive := index == tabIndex
		node := tab.Node

		// Don't attach to the node unless we were otherwise attached
		// to this tabs node
		if isActive && isAttached {
			node = L.AttachFirst(tab.Node)
		}

		newTabs = append(newTabs, L.Tab{
			Active: isActive,
			Name:   tab.Name,
			Node:   node,
		})
	}

	config.Tabs = newTabs
	t.Publish(L.NodeChangeEvent{Config: config})
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
	c := L.NewComputable(ctx)
	tabs := &Tabs{
		Computable:      c,
		UpdatePublisher: mux.NewPublisher(),
		screen:          screen,
		size:            geom.DEFAULT_SIZE,
		render:          taro.NewRenderer(),
	}

	go tabs.poll(tabs.Ctx())

	return tabs
}
