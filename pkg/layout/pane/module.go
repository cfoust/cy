package pane

import (
	"context"
	"fmt"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/style"
	"github.com/cfoust/cy/pkg/geom/tty"
	L "github.com/cfoust/cy/pkg/layout"
	"github.com/cfoust/cy/pkg/mux"
	S "github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/params"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	"github.com/sasha-s/go-deadlock"
)

type Pane struct {
	util.Lifetime
	deadlock.RWMutex
	*mux.UpdatePublisher

	params *params.Parameters
	tree   *tree.Tree
	server *server.Server

	config L.PaneType

	size geom.Size
	id   *tree.NodeID

	attachment *util.Lifetime
	screen     mux.Screen

	isAttached   bool
	removeOnExit bool
}

var _ mux.Screen = (*Pane)(nil)
var _ L.Reusable = (*Pane)(nil)

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

	p.config.Attached = true
	p.Publish(L.NodeChangeEvent{
		Config: p.config,
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

	// When the tree node is removed (ie by (tree/kill)) we need to tell
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
				p.Publish(L.NodeRemoveEvent{})
				return
			}

			// Keep the pane around, just detach from it in the
			// layout
			newConfig.ID = nil
			p.Publish(L.NodeChangeEvent{
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

				p.Publish(L.NodeRemoveEvent{})
			}
		}
	}()

	p.Notify()
	return nil
}

func (p *Pane) applyConfig(config L.PaneType) {
	p.config = config
	p.isAttached = config.Attached

	if config.RemoveOnExit != nil {
		p.removeOnExit = *config.RemoveOnExit
	} else {
		p.removeOnExit = p.params.RemovePaneOnExit()
	}
}

func (p *Pane) Apply(node L.NodeType) (bool, error) {
	config, ok := node.(L.PaneType)
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

func New(
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
	}

	p.setID(nil)

	return p
}
