package layout

import (
	"context"
	"fmt"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/layout/prop"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/params"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/sasha-s/go-deadlock"
)

type BarNode struct {
	Text   *prop.String
	Bottom bool
	Node   Node
}

var _ Node = (*BarNode)(nil)

func (n *BarNode) Type() NodeType {
	return NodeTypeBar
}

func (n *BarNode) IsAttached() bool {
	return n.Node.IsAttached()
}

func (n *BarNode) Children() (nodes []Node) {
	return []Node{n.Node}
}

func (n *BarNode) SetChild(index int, node Node) {
	if index == 0 {
		n.Node = node
	}
}

func (n *BarNode) Clone() Node {
	cloned := *n
	cloned.Node = n.Node.Clone()
	return &cloned
}

func (n *BarNode) Validate() error {
	if n.Node == nil {
		return ErrChildNil
	}

	return n.Node.Validate()
}

func (n *BarNode) MarshalJanet() interface{} {
	return struct {
		Type   janet.Keyword
		Text   *prop.String
		Bottom bool
		Node   interface{}
	}{
		Type:   NodeKeywordBar,
		Text:   n.Text,
		Bottom: n.Bottom,
		Node:   n.Node.MarshalJanet(),
	}
}

func (n *BarNode) UnmarshalJanet(value *janet.Value) (Node, error) {
	type barArgs struct {
		Text   *prop.String
		Bottom *bool
		Node   *janet.Value
	}
	args := barArgs{}
	err := value.Unmarshal(&args)
	if err != nil {
		return nil, err
	}

	node, err := unmarshalNode(args.Node)
	if err != nil {
		return nil, err
	}

	if args.Text == nil {
		return nil, fmt.Errorf(
			":bar node missing :text",
		)
	}

	type_ := BarNode{
		Node: node,
		Text: args.Text,
	}

	if args.Bottom != nil {
		type_.Bottom = *args.Bottom
	}

	return &type_, nil
}

func (n *BarNode) VisibleChildren() (nodes []Node) {
	return n.Children()
}

func (n *BarNode) SetVisibleChild(index int, node Node) {
	n.SetChild(index, node)
}

func (n *BarNode) Screen(
	ctx context.Context,
	tree *tree.Tree,
	server *server.Server,
	params *params.Parameters,
	children []Reusable,
) Reusable {
	return NewBar(ctx, children[0])
}

type Bar struct {
	*Computable
	deadlock.RWMutex
	*mux.UpdatePublisher
	render     *taro.Renderer
	screen     mux.Screen
	size       geom.Size
	inner, bar geom.Rect
	config     *BarNode
}

var _ mux.Screen = (*Bar)(nil)
var _ Reusable = (*Bar)(nil)

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
	layout := New(config.Node)
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

func (b *Bar) Apply(node Node) (bool, error) {
	config, ok := node.(*BarNode)
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
			if _, ok := event.(NodeChangeEvent); ok {
				continue
			}
			b.Publish(event)
		}
	}
}

func NewBar(
	ctx context.Context,
	screen mux.Screen,
) *Bar {
	c := NewComputable(ctx)
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
