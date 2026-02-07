package engine

import (
	"context"
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
	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
	"github.com/sasha-s/go-deadlock"
)

// screenNode is a node in the tree of layout nodes that have already been
// turned into Screens. It holds on to the Screen and the configuration that
// was used to create it originally, along with references to this layout
// node's children.
type screenNode struct {
	util.Lifetime
	Screen   L.Reusable
	Config   L.Node
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
	sleep   time.Duration
}

func (t *throttle) poll(ctx context.Context) {
	for {
		select {
		case <-ctx.Done():
			return
		case <-t.wait:
			time.Sleep(t.sleep)
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
	t.ready = time.Now().Add(t.sleep)
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
		sleep:   time.Second / time.Duration(maxFps),
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

	context interface{}
	params  *params.Parameters
	tree    *tree.Tree
	server  *server.Server
	log     zerolog.Logger

	size     geom.Size
	screen   mux.Screen
	layout   L.Node
	existing *screenNode
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
					&style.Borders[i],
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
	changedNode *screenNode,
	newConfig L.Node,
) error {
	l.Lock()
	defer l.Unlock()

	layout := l.layout

	// If the new configuration changes the attachment point, we need to
	// detach from whatever node we're currently attached to
	if layout.IsAttached() {
		layout = L.Detach(layout.Clone())
	}

	layout = applyNodeChange(
		l.existing,
		changedNode,
		layout,
		newConfig,
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
	return l.set(L.RemoveAttached(l.layout.Clone()))
}

func (l *LayoutEngine) set(layout L.Node) error {
	node, err := l.updateNode(
		l.Ctx(),
		l.existing,
		layout,
	)
	if err != nil {
		return err
	}

	var (
		screen     = node.Screen
		reusedRoot = l.existing == node
	)

	_ = screen.Resize(l.size)

	l.existing = node
	l.layout = layout
	l.screen = screen

	defer l.Notify()

	if reusedRoot {
		return nil
	}

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

func WithContext(context interface{}) Setting {
	return func(l *LayoutEngine) {
		l.context = context
	}
}

func WithLogger(logger zerolog.Logger) Setting {
	return func(l *LayoutEngine) {
		l.log = logger
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
		log:             log.Logger,
	}

	for _, setting := range settings {
		setting(engine)
	}

	return engine
}
