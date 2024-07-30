package layout

import (
	"context"
	"fmt"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/params"
	"github.com/cfoust/cy/pkg/util"

	"github.com/sasha-s/go-deadlock"
)

// reusable is used to describe a Screen that has a configuration that can
// change. Often a Screen does not actually need to be created from scratch
// when the corresponding layout node changes; it can just be updated. The
// reuse function checks whether the Screen can be updated to match the new
// configuration and updates it if possible.
type reusable interface {
	mux.Screen
	reuse(NodeType) (bool, error)
}

// screenNode is a node in the tree of layout nodes that have already been
// turned into Screens. It holds on to the Screen and the configuration that
// was used to create it originally, along with references to this layout
// node's children.
type screenNode struct {
	util.Lifetime
	Screen   reusable
	Config   NodeType
	Children []*screenNode
}

// LayoutEngine is a Screen that renders a declarative layout that consists of
// other Screens. When the layout changes, LayoutEngine detects which Screens
// in the layout can be reused and which must be created from scratch.
type LayoutEngine struct {
	util.Lifetime
	deadlock.RWMutex
	*mux.UpdatePublisher

	params *params.Parameters
	tree   *tree.Tree
	server *server.Server

	size           geom.Size
	layout         NodeType
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

	state := screen.State()
	size := state.Image.Size()

	for i := 0; i < len(borders); i++ {
		for row := 0; row < size.R; row++ {
			for col := 0; col < size.C; col++ {
				fillBorder(
					state.Image,
					row, col, size.R, size.C,
					borders[i],
				)
			}
		}
	}

	return state
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

// handleChange changes the layout that was used to create node to the
// configuration specified by config.
//
// Some nodes need to be able to change their own configuration in response to
// user input. A good example of this is when the user clicks on an inactive
// pane to focus it again. The Screen at that location detects the click and
// indicates to the LayoutEngine that it should attach to it.
func (l *LayoutEngine) handleChange(
	node *screenNode,
	config NodeType,
) error {
	l.Lock()
	defer l.Unlock()

	layout := l.layout

	// If the new configuration changes the attachment point, we need to
	// detach from whatever node we're currently attached to
	if isAttached(config) {
		layout = detach(layout)
	}

	layout = applyNodeChange(
		l.existing,
		node,
		layout,
		config,
	)

	err := validateTree(layout)
	if err != nil {
		return err
	}

	return l.set(layout)
}

// removeAttached intelligently removes the currently attached pane from the
// layout. This works in the same way tmux does, but it's more generalized. For
// example, if the attached pane were a direct child of a split, that split
// would be removed and replaced by the node in that split's other "branch".
func (l *LayoutEngine) removeAttached() error {
	l.Lock()
	defer l.Unlock()
	return l.set(removeAttached(l.layout))
}

// createNode takes a layout node and (recursively) creates a new screenNode
// that corresponds to that layout node's configuration.
func (l *LayoutEngine) createNode(
	ctx context.Context,
	config NodeType,
) (*screenNode, error) {
	nodeLifetime := util.NewLifetime(ctx)
	node := &screenNode{
		Lifetime: nodeLifetime,
		Config:   config,
	}

	var err error
	switch config := config.(type) {
	case PaneType:
		err = l.createPane(node, config)
	case MarginsType:
		err = l.createMargins(node, config)
	case SplitType:
		err = l.createSplit(node, config)
	default:
		err = fmt.Errorf("unimplemented screen")
	}

	if err != nil {
		return nil, err
	}

	go func() {
		updates := node.Screen.Subscribe(node.Ctx())
		for {
			select {
			case msg := <-updates.Recv():
				switch msg := msg.(type) {
				case nodeChangeEvent:
					// TODO(cfoust): 07/30/24 error
					// handling
					l.handleChange(node, msg.Config)
				case nodeRemoveEvent:
					l.removeAttached()
				}
			case <-node.Ctx().Done():
				return
			}
		}
	}()

	return node, err
}

type updateNode struct {
	Config NodeType
	Node   *screenNode
}

// updateNode attempts to reuse the given screenNode to match the provided
// layout node's configuration if it is possible, or creates a new screenNode
// if it is not.
func (l *LayoutEngine) updateNode(
	ctx context.Context,
	config NodeType,
	current *screenNode,
) (*screenNode, error) {
	if current == nil {
		return l.createNode(ctx, config)
	}

	canReuse, err := current.Screen.reuse(config)
	if err != nil {
		return nil, err
	}

	if !canReuse {
		current.Cancel()
		return l.createNode(ctx, config)
	}

	var updates []updateNode
	switch node := config.(type) {
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
		child, err := l.updateNode(
			current.Ctx(),
			update.Config,
			update.Node,
		)
		if err != nil {
			return nil, err
		}

		if update.Node.Screen != child.Screen {
			current.Cancel()
			return l.createNode(ctx, config)
		}

		children = append(children, child)
	}

	current.Config = config
	current.Children = children
	return current, nil
}

func (l *LayoutEngine) set(layout NodeType) error {
	node, err := l.updateNode(
		l.Ctx(),
		layout,
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
				switch msg := msg.(type) {
				case nodeChangeEvent:
					// don't emit these
				default:
					l.Publish(msg)
				}
			case <-node.Ctx().Done():
				return
			}
		}
	}()

	l.Notify()
	return nil
}

// Set changes the Layout rendered by this LayoutEngine by reusing as many
// existing Screens as it can.
func (l *LayoutEngine) Set(layout Layout) error {
	err := validateTree(layout.root)
	if err != nil {
		return err
	}

	l.Lock()
	defer l.Unlock()

	return l.set(layout.root)
}

// Get gets the Layout this LayoutEngine is rendering.
func (l *LayoutEngine) Get() Layout {
	l.RLock()
	layout := l.layout
	l.RUnlock()
	return New(layout)
}

type Setting func(*LayoutEngine)

func WithParams(params *params.Parameters) Setting {
	return func(l *LayoutEngine) {
		l.params = params
	}
}

func NewLayoutEngine(
	ctx context.Context,
	tree *tree.Tree,
	muxServer *server.Server,
	settings ...Setting,
) *LayoutEngine {
	lifetime := util.NewLifetime(ctx)
	engine := &LayoutEngine{
		Lifetime:        lifetime,
		UpdatePublisher: mux.NewPublisher(),
		tree:            tree,
		server:          muxServer,
		params:          params.New(),
	}

	for _, setting := range settings {
		setting(engine)
	}

	return engine
}
