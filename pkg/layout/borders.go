package layout

import (
	"context"
	"fmt"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/layout/prop"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/charmbracelet/lipgloss"
	"github.com/sasha-s/go-deadlock"
)

type BordersNode struct {
	Title       *prop.String
	TitleBottom *prop.String
	Border      *prop.Border
	BorderFg    *prop.Color
	BorderBg    *prop.Color
	Node        Node
}

var _ Node = (*BordersNode)(nil)

func (n *BordersNode) Type() NodeType {
	return NodeTypeBorders
}

func (n *BordersNode) IsAttached() bool {
	return n.Node.IsAttached()
}

func (n *BordersNode) Children() (nodes []Node) {
	return []Node{n.Node}
}

func (n *BordersNode) SetChild(index int, node Node) {
	if index == 0 {
		n.Node = node
	}
}

func (n *BordersNode) Clone() Node {
	cloned := &(*n)
	cloned.Node = n.Node.Clone()
	return cloned
}

func (n *BordersNode) Validate() error {
	return n.Node.Validate()
}

func (n *BordersNode) MarshalJanet() interface{} {
	return struct {
		Type        janet.Keyword
		Title       *prop.String
		TitleBottom *prop.String
		Border      *prop.Border
		BorderFg    *prop.Color
		BorderBg    *prop.Color
		Node        interface{}
	}{
		Type:        KEYWORD_BORDERS,
		Title:       n.Title,
		TitleBottom: n.TitleBottom,
		Border:      n.Border,
		BorderFg:    n.BorderFg,
		BorderBg:    n.BorderBg,
		Node:        marshalNode(n.Node),
	}
}

func (n *BordersNode) UnmarshalJanet(value *janet.Value) (Node, error) {
	type borderArgs struct {
		Title       *prop.String
		TitleBottom *prop.String
		Border      *prop.Border
		Node        *janet.Value
		BorderBg    *prop.Color
		BorderFg    *prop.Color
	}
	args := borderArgs{}
	err := value.Unmarshal(&args)
	if err != nil {
		return nil, err
	}

	node, err := unmarshalNode(args.Node)
	if err != nil {
		return nil, err
	}

	type_ := BordersNode{
		Title:       args.Title,
		TitleBottom: args.TitleBottom,
		Node:        node,
		Border:      args.Border,
		BorderFg:    args.BorderFg,
	}

	if type_.Border != nil {
		if border, ok := type_.Border.Static(); ok && border.None() {
			return nil, fmt.Errorf(
				"type :border does not support :border=:none, use :hidden instead",
			)
		}
	} else {
		type_.Border = defaultBorder
	}

	return &type_, nil
}

type Borders struct {
	*Computable
	deadlock.RWMutex
	*mux.UpdatePublisher
	render *taro.Renderer
	screen mux.Screen
	size   geom.Size
	inner  geom.Rect
	config *BordersNode
}

var _ mux.Screen = (*Borders)(nil)
var _ Reusable = (*Borders)(nil)

func (l *Borders) Apply(node Node) (bool, error) {
	config, ok := node.(*BordersNode)
	if !ok {
		return false, nil
	}

	l.Lock()
	defer l.Unlock()

	l.config = config

	for _, prop := range []prop.Presettable{
		config.Title,
		config.TitleBottom,
		config.Border,
		config.BorderFg,
		config.BorderBg,
	} {
		prop.SetLogger(l.Logger)
	}

	// Some properties only get the layout, so they can be preset
	layout := New(config.Node)
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
	}

	// But Title and TitleBottom depend on the dimensions of the node
	for _, prop := range []prop.Presettable{
		config.Title,
		config.TitleBottom,
	} {
		prop.ClearCache()
	}

	return true, nil
}

func (l *Borders) Kill() {
	l.RLock()
	screen := l.screen
	l.RUnlock()

	screen.Kill()
}

func (l *Borders) State() *tty.State {
	l.Lock()
	defer l.Unlock()

	var (
		inner  = l.inner
		size   = l.size
		config = l.config
	)

	innerState := l.screen.State()
	state := tty.New(size)

	tty.Copy(inner.Position, state, innerState)

	borderStyle := lipgloss.RoundedBorder()
	if value, ok := config.Border.GetPreset(); ok {
		borderStyle = value.Border
	}

	boxStyle := l.render.NewStyle().
		Border(borderStyle).
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

	l.render.RenderAt(state.Image, 0, 0, boxStyle.Render(""))
	titleSize := geom.Vec2{
		R: 1,
		C: inner.Size.C,
	}

	layout := New(config.Node)
	if value, ok := config.Title.Get(
		l.Ctx(),
		l.Context.Context(),
		titleSize,
		&layout,
	); ok {
		l.render.RenderAt(
			state.Image,
			0, 1,
			l.render.NewStyle().
				MaxWidth(inner.Size.C).
				Render(value),
		)
	}

	if value, ok := config.TitleBottom.Get(
		l.Ctx(),
		l.Context.Context(),
		titleSize,
		&layout,
	); ok {
		l.render.RenderAt(
			state.Image,
			size.R-1, 1,
			l.render.NewStyle().
				MaxWidth(inner.Size.C).
				Render(value),
		)
	}

	return state
}

func (l *Borders) Send(msg mux.Msg) {
	l.RLock()
	inner := l.inner
	l.RUnlock()
	l.screen.Send(taro.TranslateMouseMessage(
		msg,
		-inner.Position.C,
		-inner.Position.R,
	))
}

func (l *Borders) Size() geom.Size {
	l.RLock()
	defer l.RUnlock()
	return l.size
}

func (l *Borders) poll(ctx context.Context) {
	updates := l.screen.Subscribe(ctx)

	for {
		select {
		case <-ctx.Done():
			return
		case event := <-updates.Recv():
			if _, ok := event.(NodeChangeEvent); ok {
				continue
			}
			l.Publish(event)
		}
	}
}

func (l *Borders) recalculate() error {
	l.RLock()
	size := l.size
	l.RUnlock()

	inner := geom.Rect{
		Position: geom.UnitVec2,
		Size: geom.Vec2{
			R: geom.Max(1, size.R-2),
			C: geom.Max(1, size.C-2),
		},
	}

	l.Lock()
	l.inner = inner
	// Title and TitleBottom depend on the dimensions of the node, so they
	// must be reset when the dimensions change
	for _, prop := range []prop.Presettable{
		l.config.Title,
		l.config.TitleBottom,
	} {
		prop.ClearCache()
	}
	l.Unlock()

	err := l.screen.Resize(inner.Size)
	if err != nil {
		return err
	}

	return nil
}

func (l *Borders) Resize(size geom.Size) error {
	l.Lock()
	l.size = size
	l.Unlock()
	return l.recalculate()
}

func NewBorders(ctx context.Context, screen mux.Screen) *Borders {
	c := NewComputable(ctx)
	borders := &Borders{
		Computable:      c,
		UpdatePublisher: mux.NewPublisher(),
		size:            geom.DEFAULT_SIZE,
		screen:          screen,
		render:          taro.NewRenderer(),
	}

	go borders.poll(borders.Ctx())

	return borders
}
