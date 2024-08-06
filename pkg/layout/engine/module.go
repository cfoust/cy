package engine

import (
	"context"
	"fmt"
	"time"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	L "github.com/cfoust/cy/pkg/layout"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/params"
	"github.com/cfoust/cy/pkg/style"
	"github.com/cfoust/cy/pkg/util"
	tea "github.com/charmbracelet/bubbletea"

	"github.com/sasha-s/go-deadlock"
)

// screenNode is a node in the tree of layout nodes that have already been
// turned into Screens. It holds on to the Screen and the configuration that
// was used to create it originally, along with references to this layout
// node's children.
type screenNode struct {
	util.Lifetime
	Screen   L.Reusable
	Config   L.NodeType
	Children []*screenNode
}

// throttle is a throttled update publisher that only allows nil messages to
// be published at a maximum frame rate. This is particularly useful for the
// LayoutEngine because any time a node tells its subscribers that its state
// has changed, all of its ancestors are also notified, which is usually
// desirable, but causes lots of unnecessary renders in the top-level
// LayoutEngine.
type throttle struct {
	deadlock.RWMutex
	publish *mux.UpdatePublisher
	wait    chan interface{}
	ready   time.Time
	sleepMs time.Duration
}

func (t *throttle) poll(ctx context.Context) {
	for {
		select {
		case <-ctx.Done():
			return
		case <-t.wait:
			time.Sleep(t.sleepMs)
			t.publish.Notify()
		}
	}
}

func (t *throttle) Publish(value tea.Msg) {
	if value != nil {
		t.publish.Publish(value)
		return
	}

	t.RLock()
	var (
		ready = t.ready
	)
	t.RUnlock()

	if time.Now().Before(ready) {
		return
	}

	t.Lock()
	t.ready = time.Now().Add(t.sleepMs)
	t.Unlock()

	t.wait <- nil
}

func newThrottle(
	ctx context.Context,
	p *mux.UpdatePublisher,
	maxFps int,
) *throttle {
	t := &throttle{
		publish: p,
		wait:    make(chan interface{}),
		sleepMs: time.Second / time.Duration(maxFps),
		ready:   time.Now(),
	}

	go t.poll(ctx)

	return t
}

// LayoutEngine is a Screen that renders a declarative layout that consists of
// other Screens. When the layout changes, LayoutEngine detects which Screens
// in the layout can be reused and which must be created from scratch.
type LayoutEngine struct {
	util.Lifetime
	deadlock.RWMutex
	*mux.UpdatePublisher
	throttle *throttle

	params *params.Parameters
	tree   *tree.Tree
	server *server.Server

	size           geom.Size
	layout         L.NodeType
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

	for i := 0; i < len(style.Borders); i++ {
		if !style.Borders[i].Smoothable() {
			continue
		}

		for row := 0; row < size.R; row++ {
			for col := 0; col < size.C; col++ {
				style.FillBorder(
					state.Image,
					row, col, size.R, size.C,
					style.Borders[i],
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
	config L.NodeType,
) error {
	l.Lock()
	defer l.Unlock()

	layout := l.layout

	// If the new configuration changes the attachment point, we need to
	// detach from whatever node we're currently attached to
	if L.IsAttached(config) {
		layout = L.Detach(layout)
	}

	layout = applyNodeChange(
		l.existing,
		node,
		layout,
		config,
	)

	err := L.ValidateTree(layout)
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
	return l.set(L.RemoveAttached(l.layout))
}

// createNode takes a layout node and (recursively) creates a new screenNode
// that corresponds to that layout node's configuration.
func (l *LayoutEngine) createNode(
	ctx context.Context,
	config L.NodeType,
) (*screenNode, error) {
	nodeLifetime := util.NewLifetime(ctx)
	node := &screenNode{
		Lifetime: nodeLifetime,
		Config:   config,
	}

	var err error
	switch config := config.(type) {
	case L.PaneType:
		err = l.createPane(node, config)
	case L.MarginsType:
		err = l.createMargins(node, config)
	case L.SplitType:
		err = l.createSplit(node, config)
	case L.BorderType:
		err = l.createBorders(node, config)
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
				case L.NodeChangeEvent:
					// TODO(cfoust): 07/30/24 error
					// handling
					l.handleChange(node, msg.Config)
				case L.NodeRemoveEvent:
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
	Config L.NodeType
	Node   *screenNode
}

// updateNode attempts to reuse the given screenNode to match the provided
// layout node's configuration if it is possible, or creates a new screenNode
// if it is not.
func (l *LayoutEngine) updateNode(
	ctx context.Context,
	config L.NodeType,
	current *screenNode,
) (*screenNode, error) {
	if current == nil {
		return l.createNode(ctx, config)
	}

	canReuse, err := current.Screen.Apply(config)
	if err != nil {
		return nil, err
	}

	if !canReuse {
		current.Cancel()
		return l.createNode(ctx, config)
	}

	var updates []updateNode
	switch node := config.(type) {
	case L.SplitType:
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
	case L.MarginsType:
		updates = append(updates,
			updateNode{
				Config: node.Node,
				Node:   current.Children[0],
			},
		)
	case L.BorderType:
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

func (l *LayoutEngine) set(layout L.NodeType) error {
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
				case L.NodeChangeEvent:
					// don't emit these
				default:
					l.throttle.Publish(msg)
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
func (l *LayoutEngine) Set(layout L.Layout) error {
	err := L.ValidateTree(layout.Root)
	if err != nil {
		return err
	}

	l.Lock()
	defer l.Unlock()

	return l.set(layout.Root)
}

// Get gets the Layout this LayoutEngine is rendering.
func (l *LayoutEngine) Get() L.Layout {
	l.RLock()
	layout := l.layout
	l.RUnlock()
	return L.New(layout)
}

type Setting func(*LayoutEngine)

func WithParams(params *params.Parameters) Setting {
	return func(l *LayoutEngine) {
		l.params = params
	}
}

func New(
	ctx context.Context,
	tree *tree.Tree,
	muxServer *server.Server,
	settings ...Setting,
) *LayoutEngine {
	lifetime := util.NewLifetime(ctx)
	p := mux.NewPublisher()
	t := newThrottle(
		lifetime.Ctx(),
		p,
		60,
	)
	engine := &LayoutEngine{
		Lifetime:        lifetime,
		UpdatePublisher: p,
		tree:            tree,
		server:          muxServer,
		params:          params.New(),
		throttle:        t,
	}

	for _, setting := range settings {
		setting(engine)
	}

	return engine
}

// applyNodeChange replaces the configuration of the target node with
// newConfig. This is only used to allow nodes to change their own
// configurations in response to user input (for now, just mouse events.)
func applyNodeChange(
	current, target *screenNode,
	currentConfig, newConfig L.NodeType,
) L.NodeType {
	if current == target {
		return newConfig
	}

	switch currentConfig := currentConfig.(type) {
	case L.PaneType:
		return currentConfig
	case L.SplitType:
		currentConfig.A = applyNodeChange(
			current.Children[0],
			target,
			currentConfig.A,
			newConfig,
		)
		currentConfig.B = applyNodeChange(
			current.Children[1],
			target,
			currentConfig.B,
			newConfig,
		)
		return currentConfig
	case L.MarginsType:
		currentConfig.Node = applyNodeChange(
			current.Children[0],
			target,
			currentConfig.Node,
			newConfig,
		)
		return currentConfig
	case L.BorderType:
		currentConfig.Node = applyNodeChange(
			current.Children[0],
			target,
			currentConfig.Node,
			newConfig,
		)
		return currentConfig
	}

	return currentConfig
}