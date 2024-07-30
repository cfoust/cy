package layout

import (
	"context"
	"fmt"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	"github.com/sasha-s/go-deadlock"
)

type Pane struct {
	util.Lifetime
	deadlock.RWMutex
	*mux.UpdatePublisher

	tree   *tree.Tree
	server *server.Server

	config PaneType

	size geom.Size
	id   *tree.NodeID

	attachment *util.Lifetime
	screen     mux.Screen

	isAttached bool
}

var _ mux.Screen = (*Pane)(nil)
var _ reusable = (*Pane)(nil)

func (p *Pane) Send(msg mux.Msg) {
	p.RLock()
	defer p.RUnlock()

	if !p.isAttached {
		switch msg := msg.(type) {
		case taro.MouseMsg:
			if msg.Type != taro.MousePress || msg.Button != taro.MouseLeft || !msg.Down {
				return
			}

			bounds := geom.Rect{
				Size: p.size,
			}
			if !bounds.Contains(msg.Vec2) {
				return
			}

			p.config.Attached = true
			p.Publish(nodeChangeEvent{
				Config: p.config,
			})
		}
		return
	}

	if p.screen == nil {
		return
	}

	p.screen.Send(msg)
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

	state := p.screen.State()
	if !p.isAttached && state.CursorVisible {
		cursor := state.Cursor
		state.CursorVisible = false
		state.Image[cursor.R][cursor.C].BG = 8
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
			true,
			"disconnected",
		), nil
	}

	treeNode, ok := p.tree.NodeById(*id)
	if !ok {
		return NewStatic(
			ctx,
			true,
			fmt.Sprintf("node %d not found", *id),
		), nil
	}

	pane, ok := treeNode.(*tree.Pane)
	if !ok {
		return NewStatic(
			ctx,
			true,
			fmt.Sprintf("node %d is not a pane", *id),
		), nil
	}

	client := p.server.AddClient(ctx, p.size)
	client.Attach(ctx, pane.Screen())
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
				p.Publish(event)
			}
		}
	}()

	p.Notify()
	return nil
}

func (p *Pane) reuse(node NodeType) (bool, error) {
	config, ok := node.(PaneType)
	if !ok {
		return false, nil
	}

	p.Lock()
	p.config = config
	p.isAttached = config.Attached
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

func (l *LayoutEngine) createPane(
	node *screenNode,
	config PaneType,
) error {
	pane := NewPane(
		node.Ctx(),
		l.tree,
		l.server,
	)

	pane.config = config
	pane.isAttached = config.Attached

	err := pane.setID(config.ID)
	if err != nil {
		return err
	}

	node.Screen = pane
	return nil
}

func NewPane(
	ctx context.Context,
	tree *tree.Tree,
	server *server.Server,
) *Pane {
	lifetime := util.NewLifetime(ctx)
	p := &Pane{
		Lifetime:        lifetime,
		UpdatePublisher: mux.NewPublisher(),
		tree:            tree,
		server:          server,
	}

	p.setID(nil)

	return p
}
