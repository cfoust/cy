package layout

import (
	"context"
	"fmt"
	"sync/atomic"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux"
	S "github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/params"
	"github.com/cfoust/cy/pkg/style"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	"github.com/sasha-s/go-deadlock"
)

var nextRegionId atomic.Uint32

type PaneNode struct {
	Attached     bool
	RemoveOnExit *bool
	ID           *tree.NodeID
}

var _ Node = (*PaneNode)(nil)

func (p *PaneNode) Type() NodeType {
	return NodeTypePane
}

func (p *PaneNode) IsAttached() bool {
	return p.Attached
}

func (p *PaneNode) Children() (nodes []Node) {
	return
}

func (p *PaneNode) SetChild(index int, node Node) {
}

func (p *PaneNode) Clone() Node {
	cloned := *p
	return &cloned
}

func (p *PaneNode) Validate() error {
	return nil
}

func (p *PaneNode) MarshalJanet() interface{} {
	return struct {
		Type         janet.Keyword
		Attached     bool
		ID           *tree.NodeID
		RemoveOnExit *bool
	}{
		Type:         NodeKeywordPane,
		Attached:     p.Attached,
		ID:           p.ID,
		RemoveOnExit: p.RemoveOnExit,
	}
}

func (p *PaneNode) UnmarshalJanet(value *janet.Value) (Node, error) {
	type paneArgs struct {
		Attached     *bool
		RemoveOnExit *bool
		ID           *tree.NodeID
	}
	args := paneArgs{}

	err := value.Unmarshal(&args)
	if err != nil {
		return nil, err
	}
	type_ := PaneNode{
		ID: args.ID,
	}

	if args.Attached != nil {
		type_.Attached = *args.Attached
	}

	if args.RemoveOnExit != nil {
		type_.RemoveOnExit = args.RemoveOnExit
	}

	return &type_, err
}

func (n *PaneNode) VisibleChildren() (nodes []Node) {
	return n.Children()
}

func (n *PaneNode) SetVisibleChild(index int, node Node) {
	n.SetChild(index, node)
}

func (n *PaneNode) Screen(
	ctx context.Context,
	tree *tree.Tree,
	server *server.Server,
	params *params.Parameters,
	children []Reusable,
) Reusable {
	return NewPane(
		ctx,
		tree,
		server,
		params,
	)
}

type Pane struct {
	util.Lifetime
	deadlock.RWMutex
	*mux.UpdatePublisher

	params *params.Parameters
	tree   *tree.Tree
	server *server.Server

	config *PaneNode

	size geom.Size
	id   *tree.NodeID

	attachment *util.Lifetime
	screen     mux.Screen

	isAttached   bool
	removeOnExit bool

	// A unique ID for this Pane that is _only_ used for thumbs
	regionId uint32
}

var _ mux.Screen = (*Pane)(nil)
var _ Reusable = (*Pane)(nil)

func (p *Pane) Send(msg mux.Msg) {
	p.RLock()
	defer p.RUnlock()

	if p.screen == nil {
		return
	}

	if p.isAttached {
		p.screen.Send(msg)
		return
	}

	mouseMsg, ok := msg.(taro.MouseMsg)
	if !ok {
		return
	}

	if mouseMsg.Type != taro.MousePress || mouseMsg.Button != taro.MouseLeft || mouseMsg.Down {
		return
	}

	bounds := geom.Rect{
		Size: p.size,
	}
	if !bounds.Contains(mouseMsg.Vec2) {
		return
	}

	newConfig := p.config.Clone().(*PaneNode)
	newConfig.Attached = true
	p.Publish(NodeChangeEvent{
		Config: newConfig,
	})
}

func (p *Pane) Kill() {
	p.Cancel()
}

func (p *Pane) State() *tty.State {
	p.RLock()
	defer p.RUnlock()

	if p.screen == nil {
		return tty.New(p.size)
	}

	state := p.screen.State().Clone()
	if !p.isAttached && state.CursorVisible {
		cursor := state.Cursor
		state.CursorVisible = false
		style.GhostCursor(state.Image, cursor.R, cursor.C)
	}

	// Store a unique number into the WriteID so consumers of the
	// LayoutEngine can detect pane regions
	writeId := emu.WriteID(p.regionId)
	size := state.Image.Size()
	for row := range size.R {
		for col := range size.C {
			state.Image[row][col].Write = writeId
		}
	}

	return state
}

func (p *Pane) Resize(size geom.Size) error {
	p.Lock()
	p.size = size
	screen := p.screen
	p.Unlock()

	if screen == nil {
		return nil
	}

	return screen.Resize(size)
}

func (p *Pane) attach(
	ctx context.Context,
	id *tree.NodeID,
) (mux.Screen, error) {
	if id == nil {
		return NewStatic(
			ctx,
			p.params.Animate(),
			"disconnected from pane",
		), nil
	}

	treeNode, ok := p.tree.NodeById(*id)
	if !ok {
		return NewStatic(
			ctx,
			p.params.Animate(),
			fmt.Sprintf("node %d not found", *id),
		), nil
	}

	pane, ok := treeNode.(*tree.Pane)
	if !ok {
		return NewStatic(
			ctx,
			p.params.Animate(),
			fmt.Sprintf("node %d is not a pane", *id),
		), nil
	}

	client := p.server.AddClient(ctx, p.size)
	client.Attach(ctx, pane.Screen())

	// When the tree node is removed (ie by (tree/rm)) we need to tell
	// the layout engine to remove the reference to that NodeID from the
	// config for this pane.
	go func() {
		select {
		case <-ctx.Done():
		case <-pane.Ctx().Done():
			p.RLock()
			var (
				newConfig    = p.config
				removeOnExit = p.removeOnExit
				isAttached   = p.isAttached
			)
			p.RUnlock()

			// Remove the node from the tree
			if removeOnExit && isAttached {
				p.Publish(NodeRemoveEvent{})
				return
			}

			// Keep the pane around, just detach from it in the
			// layout
			newConfig.ID = nil
			p.Publish(NodeChangeEvent{
				Config: newConfig,
			})
		}
	}()

	return client, nil
}

func (p *Pane) setID(id *tree.NodeID) error {
	p.Lock()
	defer p.Unlock()

	if p.attachment != nil {
		p.attachment.Cancel()
	}

	attachment := util.NewLifetime(p.Ctx())
	screen, err := p.attach(attachment.Ctx(), id)
	if err != nil {
		return err
	}

	p.id = id
	p.attachment = &attachment
	p.screen = screen

	go func() {
		subscriber := screen.Subscribe(attachment.Ctx())
		changes := subscriber.Recv()

		for {
			select {
			case <-attachment.Ctx().Done():
				return
			case event := <-changes:
				exitEvent, ok := event.(S.ExitEvent)
				if !ok {
					p.Publish(event)
					continue
				}

				p.RLock()
				var (
					removeOnExit = p.removeOnExit
					isAttached   = p.isAttached
				)
				p.RUnlock()

				if !removeOnExit || !isAttached || exitEvent.Errored {
					continue
				}

				p.Publish(NodeRemoveEvent{})
			}
		}
	}()

	p.Notify()
	return nil
}

func (p *Pane) applyConfig(config *PaneNode) {
	p.config = config
	p.isAttached = config.Attached

	if config.RemoveOnExit != nil {
		p.removeOnExit = *config.RemoveOnExit
	} else {
		p.removeOnExit = p.params.RemovePaneOnExit()
	}
}

func (p *Pane) Apply(node Node) (bool, error) {
	config, ok := node.(*PaneNode)
	if !ok {
		return false, nil
	}

	p.Lock()
	p.applyConfig(config)
	p.Unlock()

	oldID := p.id
	newID := config.ID

	if oldID != nil && newID != nil && *oldID == *newID {
		return true, nil
	}

	if config.ID != p.id {
		err := p.setID(config.ID)
		if err != nil {
			return false, err
		}
	}

	return true, nil
}

func NewPane(
	ctx context.Context,
	tree *tree.Tree,
	server *server.Server,
	params *params.Parameters,
) *Pane {
	lifetime := util.NewLifetime(ctx)
	p := &Pane{
		Lifetime:        lifetime,
		UpdatePublisher: mux.NewPublisher(),
		tree:            tree,
		server:          server,
		params:          params,
		regionId:        nextRegionId.Add(1),
	}

	_ = p.setID(nil)

	return p
}
