package layout

import (
	"context"
	"fmt"

	"github.com/cfoust/cy/pkg/geom"
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

type ColorMapNode struct {
	Map  *prop.ColorMap
	Node Node
}

var _ Node = (*ColorMapNode)(nil)

func (n *ColorMapNode) Type() NodeType {
	return NodeTypeColorMap
}

func (n *ColorMapNode) IsAttached() bool {
	return n.Node.IsAttached()
}

func (n *ColorMapNode) Children() (nodes []Node) {
	return []Node{n.Node}
}

func (n *ColorMapNode) SetChild(index int, node Node) {
	if index == 0 {
		n.Node = node
	}
}

func (n *ColorMapNode) Clone() Node {
	cloned := *n
	cloned.Node = n.Node.Clone()
	return &cloned
}

func (n *ColorMapNode) Validate() error {
	if n.Node == nil {
		return ErrChildNil
	}

	return n.Node.Validate()
}

func (n *ColorMapNode) MarshalJanet() interface{} {
	return struct {
		Type janet.Keyword
		Map  *prop.ColorMap
		Node interface{}
	}{
		Type: NodeKeywordColorMap,
		Map:  n.Map,
		Node: n.Node.MarshalJanet(),
	}
}

func (n *ColorMapNode) UnmarshalJanet(value *janet.Value) (Node, error) {
	type colormapArgs struct {
		Map  *prop.ColorMap
		Node *janet.Value
	}

	args := colormapArgs{}
	err := value.Unmarshal(&args)
	if err != nil {
		return nil, err
	}

	node, err := unmarshalNode(args.Node)
	if err != nil {
		return nil, err
	}

	if args.Map == nil {
		return nil, fmt.Errorf(
			":color-map node missing :map",
		)
	}

	return &ColorMapNode{
		Map:  args.Map,
		Node: node,
	}, nil
}

func (n *ColorMapNode) VisibleChildren() (nodes []Node) {
	return n.Children()
}

func (n *ColorMapNode) SetVisibleChild(index int, node Node) {
	n.SetChild(index, node)
}

func (n *ColorMapNode) Screen(
	ctx context.Context,
	tree *tree.Tree,
	server *server.Server,
	params *params.Parameters,
	children []Reusable,
) Reusable {
	return NewColorMap(ctx, children[0])
}

type ColorMap struct {
	*Computable
	deadlock.RWMutex
	*mux.UpdatePublisher
	render *taro.Renderer
	screen mux.Screen
	size   geom.Size
	config *ColorMapNode
}

var _ mux.Screen = (*ColorMap)(nil)
var _ Reusable = (*ColorMap)(nil)

func (l *ColorMap) Apply(node Node) (bool, error) {
	config, ok := node.(*ColorMapNode)
	if !ok {
		return false, nil
	}

	l.Lock()
	defer l.Unlock()

	l.config = config

	layout := New(config.Node)
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
			if _, ok := event.(NodeChangeEvent); ok {
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

func NewColorMap(ctx context.Context, screen mux.Screen) *ColorMap {
	c := NewComputable(ctx)
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
