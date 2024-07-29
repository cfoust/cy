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

type Pane struct {
	util.Lifetime
	deadlock.RWMutex
	*mux.UpdatePublisher

	tree   *tree.Tree
	server *server.Server

	size geom.Size
	id   *tree.NodeID

	attachment *util.Lifetime
	screen     mux.Screen
}

var _ mux.Screen = (*Pane)(nil)
var _ reusable = (*Pane)(nil)

func (p *Pane) Send(msg mux.Msg) {
	p.RLock()
	defer p.RUnlock()
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

	return p.screen.State()
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
) (*screenNode, error) {
	pane := NewPane(
		node.Ctx(),
		l.tree,
		l.server,
	)

	err := pane.setID(config.ID)
	if err != nil {
		return nil, err
	}

	node.Screen = pane
	return node, nil
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
