package layout

import (
	"context"
	"fmt"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/util"

	"github.com/sasha-s/go-deadlock"
)

type reusable interface {
	mux.Screen
	reuse(NodeType) (bool, error)
}

type screenNode struct {
	util.Lifetime
	Screen   reusable
	Config   NodeType
	Children []*screenNode
}

type LayoutEngine struct {
	util.Lifetime
	deadlock.RWMutex
	*mux.UpdatePublisher

	tree   *tree.Tree
	server *server.Server

	size           geom.Size
	layout         Layout
	layoutLifetime *util.Lifetime
	screen         mux.Screen
	existing       *screenNode
}

var _ mux.Screen = (*LayoutEngine)(nil)

func (l *LayoutEngine) Kill() {
	l.Cancel()
}

func (l *LayoutEngine) State() *tty.State {
	l.RLock()
	screen := l.screen
	l.RUnlock()

	if screen == nil {
		return tty.New(geom.DEFAULT_SIZE)
	}

	return screen.State()
}

func (l *LayoutEngine) Send(msg mux.Msg) {
	l.RLock()
	screen := l.screen
	l.RUnlock()

	if screen == nil {
		return
	}

	screen.Send(msg)
}

func (l *LayoutEngine) Resize(size geom.Size) error {
	l.Lock()
	l.size = size
	screen := l.screen
	l.Unlock()

	if screen == nil {
		return nil
	}

	return screen.Resize(size)
}

func (l *LayoutEngine) createNode(
	ctx context.Context,
	config NodeType,
) (*screenNode, error) {
	nodeLifetime := util.NewLifetime(ctx)
	node := &screenNode{
		Lifetime: nodeLifetime,
		Config:   config,
	}

	switch config := config.(type) {
	case PaneType:
		node.Screen = NewPane(
			ctx,
			l.tree,
			l.server,
		)
		return node, nil
	case MarginsType:
		marginsNode, err := l.createNode(ctx, config.Node)
		if err != nil {
			return nil, err
		}

		margins := NewMargins(ctx, marginsNode.Screen)
		margins.setSize(geom.Size{
			R: config.Rows,
			C: config.Cols,
		})
		node.Screen = margins
		node.Children = []*screenNode{marginsNode}
		return node, nil
	case SplitType:
		nodeA, err := l.createNode(ctx, config.A)
		if err != nil {
			return nil, err
		}
		nodeB, err := l.createNode(ctx, config.B)
		if err != nil {
			return nil, err
		}

		split := NewSplit(
			ctx,
			nodeA.Screen,
			nodeB.Screen,
			config.Vertical,
		)
		split.isAttachedA = isAttached(config.A)
		split.isAttachedB = isAttached(config.B)

		if config.Percent != nil {
			split.setPercent(*config.Percent)
		}

		if config.Cells != nil {
			split.setCells(*config.Cells)
		}

		node.Screen = split
		node.Children = []*screenNode{nodeA, nodeB}
		return node, nil
	}

	return nil, fmt.Errorf("unimplemented screen")
}

type updateNode struct {
	Config NodeType
	Node   *screenNode
}

func (l *LayoutEngine) updateNode(
	ctx context.Context,
	type_ NodeType,
	current *screenNode,
) (*screenNode, error) {
	if current == nil {
		return l.createNode(
			ctx,
			type_,
		)
	}

	canReuse, err := current.Screen.reuse(type_)
	if err != nil {
		return nil, err
	}

	if !canReuse {
		current.Cancel()
		return l.createNode(ctx, type_)
	}

	var updates []updateNode
	switch node := type_.(type) {
	case SplitType:
		updates = append(updates,
			updateNode{
				Config: node.A,
				Node:   current.Children[0],
			},
			updateNode{
				Config: node.B,
				Node:   current.Children[1],
			},
		)
	case MarginsType:
		updates = append(updates,
			updateNode{
				Config: node.Node,
				Node:   current.Children[0],
			},
		)
	}

	// If any of the node's children cannot be reused, we need to remake
	// the whole node. Theoretically, you could (at the cost of a lot of
	// complexity) remake only the nodes that changed, but this would
	// require careful retooling of all of the node screens to handle
	// partially changing the screens they subscribe to.
	var children []*screenNode
	for _, update := range updates {
		canReuse, err := update.Node.Screen.reuse(update.Config)
		if err != nil {
			return nil, err
		}
		if !canReuse {
			current.Cancel()
			return l.createNode(ctx, type_)
		}

		child, err := l.updateNode(
			ctx,
			update.Config,
			update.Node,
		)
		if err != nil {
			return nil, err
		}

		children = append(children, child)
	}

	current.Config = type_
	current.Children = children
	return current, nil
}

func (l *LayoutEngine) Set(layout Layout) error {
	err := validateTree(layout.root)
	if err != nil {
		return err
	}

	l.Lock()
	defer l.Unlock()

	node, err := l.updateNode(
		l.Ctx(),
		layout.root,
		l.existing,
	)
	if err != nil {
		return err
	}

	screen := node.Screen

	l.existing = node

	screen.Resize(l.size)

	l.layout = layout
	l.screen = screen

	go func() {
		updates := screen.Subscribe(node.Ctx())
		for {
			select {
			case msg := <-updates.Recv():
				l.Publish(msg)
			case <-node.Ctx().Done():
				return
			}
		}
	}()

	l.Notify()
	return nil
}

func (l *LayoutEngine) Get() Layout {
	l.RLock()
	layout := l.layout
	l.RUnlock()
	return layout
}

func NewLayoutEngine(
	ctx context.Context,
	tree *tree.Tree,
	muxServer *server.Server,
) *LayoutEngine {
	lifetime := util.NewLifetime(ctx)
	engine := &LayoutEngine{
		Lifetime:        lifetime,
		UpdatePublisher: mux.NewPublisher(),
		tree:            tree,
		server:          muxServer,
	}

	return engine
}
