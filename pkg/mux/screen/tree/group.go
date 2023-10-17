package tree

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/stream"
	"github.com/cfoust/cy/pkg/sessions"

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

func (g *Group) NewPane(
	ctx context.Context,
	stream mux.Stream,
	size geom.Vec2,
) *Pane {
	metadata := g.tree.newMetadata()
	pane := newPane(
		ctx,
		metadata.Id(),
		stream,
		"",
		size,
		g.tree.replayBinds,
		g.tree.replayEvents,
	)
	pane.metaData = metadata
	g.addNode(pane)
	return pane
}

func (g *Group) NewCmd(ctx context.Context, options stream.CmdOptions, size geom.Vec2) (*Pane, error) {
	cmd, err := stream.NewCmd(ctx, options, size)
	if err != nil {
		return nil, err
	}

	borgPath, err := sessions.GetFilename(g.tree.DataDir(), options.Directory)
	if err != nil {
		return nil, err
	}

	metadata := g.tree.newMetadata()
	pane := newPane(
		ctx,
		metadata.Id(),
		cmd,
		borgPath,
		size,
		g.tree.replayBinds,
		g.tree.replayEvents,
	)
	pane.metaData = metadata
	g.addNode(pane)
	return pane, nil
}

func (g *Group) NewGroup() *Group {
	group := &Group{
		metaData: g.tree.newMetadata(),
		tree:     g.tree,
	}
	g.addNode(group)
	return group
}
