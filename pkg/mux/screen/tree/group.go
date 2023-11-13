package tree

import (
	"context"

	"github.com/cfoust/cy/pkg/mux"

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

func (g *Group) NewPane(ctx context.Context, screen mux.Screen) *Pane {
	metadata := g.tree.newMetadata()
	pane := newPane(ctx, metadata.Id(), screen)
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
		metaData: g.tree.newMetadata(),
		tree:     g.tree,
	}
	group.params = g.params.NewChild()
	g.addNode(group)
	return group
}
