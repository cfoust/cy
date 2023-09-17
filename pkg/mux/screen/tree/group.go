package tree

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/stream"

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

func (g *Group) NewPane(
	ctx context.Context,
	stream mux.Stream,
	size geom.Vec2,
) *Pane {
	pane := newPane(ctx, stream, "test.borg", size)
	pane.metaData = g.tree.newMetadata()
	g.addNode(pane)
	return pane
}

func (g *Group) NewCmd(ctx context.Context, options stream.CmdOptions, size geom.Vec2) (*Pane, error) {
	cmd, err := stream.NewCmd(ctx, options, size)
	if err != nil {
		return nil, err
	}
	pane := newPane(ctx, cmd, "cmd.borg", size)
	pane.metaData = g.tree.newMetadata()
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
