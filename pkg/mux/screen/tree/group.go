package tree

import (
	"context"

	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/util"

	"github.com/sasha-s/go-deadlock"
)

type Group struct {
	deadlock.RWMutex
	*metaData
	tree     *Tree
	children []Node
}

var _ Node = (*Group)(nil)

func (g *Group) Children() []Node {
	g.RLock()
	defer g.RUnlock()

	return g.children
}

func (g *Group) ChildByName(name string) (node Node, ok bool) {
	g.RLock()
	children := g.children
	g.RUnlock()

	for _, child := range children {
		if child.Name() != name {
			continue
		}
		return child, true
	}

	return
}

func (g *Group) addNode(node Node) {
	g.tree.storeNode(node)
	g.Lock()
	g.children = append(g.children, node)
	g.Unlock()
}

func (g *Group) removeNode(node Node) {
	newChildren := make([]Node, 0)

	g.Lock()
	for _, child := range g.children {
		if child == node {
			continue
		}
		newChildren = append(newChildren, child)
	}
	g.children = newChildren
	g.Unlock()
}

func (g *Group) Leaves() []Node {
	return getLeaves(g)
}

// NewPaneCreator is the same as NewPane, but it gives you the NodeID before
// the Node is created and a function to call with the final Screen.
func (g *Group) NewPaneCreator(ctx context.Context) (
	Node,
	func(screen mux.Screen) *Pane,
) {
	p := &Pane{Lifetime: util.NewLifetime(ctx)}
	metadata := g.tree.newMetadata()
	metadata.params = g.params.NewChild()
	p.metaData = metadata

	return p, func(screen mux.Screen) *Pane {
		p.screen = screen
		g.addNode(p)

		go func() {
			updates := screen.Subscribe(ctx)
			for {
				select {
				case event := <-updates.Recv():
					g.tree.Publish(NodeEvent{
						Id:    metadata.Id(),
						Event: event,
					})
				case <-ctx.Done():
					return
				}
			}
		}()

		return p
	}
}

func (g *Group) NewPane(ctx context.Context, screen mux.Screen) *Pane {
	pane := newPane(ctx, screen)
	metadata := g.tree.newMetadata()
	pane.metaData = metadata
	metadata.params = g.params.NewChild()
	g.addNode(pane)

	go func() {
		updates := screen.Subscribe(ctx)
		for {
			select {
			case event := <-updates.Recv():
				g.tree.Publish(NodeEvent{
					Id:    metadata.Id(),
					Event: event,
				})
			case <-ctx.Done():
				return
			}
		}
	}()

	return pane
}

func (g *Group) NewGroup() *Group {
	group := &Group{
		tree: g.tree,
	}
	group.metaData = g.tree.newMetadata()
	group.params = g.params.NewChild()
	g.addNode(group)
	return group
}
