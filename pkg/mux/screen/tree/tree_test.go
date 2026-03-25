package tree

import (
	"context"
	"testing"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/mux/stream"
	"github.com/cfoust/cy/pkg/params"

	"github.com/stretchr/testify/require"
)

func TestRoot(t *testing.T) {
	tree := NewTree()
	require.Equal(t, NodeID(1), tree.Root().Id())
	require.Equal(t, 0, len(tree.Leaves()))
}

func emptyPane(g *Group) *Pane {
	return g.NewPane(
		context.Background(),
		screen.NewTerminal(
			context.Background(),
			stream.NewReader(),
			geom.DEFAULT_SIZE,
			params.New(),
		),
	)
}

func TestLeaves(t *testing.T) {
	tree := NewTree()
	g := tree.Root().NewGroup()
	for i := 0; i < 3; i++ {
		emptyPane(g)
	}
	require.Equal(t, 3, len(g.Children()))
	require.Equal(t, 3, len(tree.Leaves()))
}

func TestRemoveGroup(t *testing.T) {
	tree := NewTree()
	g := tree.Root().NewGroup()
	for i := 0; i < 3; i++ {
		emptyPane(g)
	}

	_ = tree.RemoveNode(g.Id())
	require.Equal(t, 0, len(tree.Leaves()))
}

func TestRemoveNode(t *testing.T) {
	tree := NewTree()
	g := tree.Root().NewGroup()
	for i := 0; i < 3; i++ {
		emptyPane(g)
	}

	child := g.Children()[1]
	_ = tree.RemoveNode(child.Id())
	require.Equal(t, 2, len(tree.Leaves()))
	require.Equal(t, 2, len(g.Children()))
}

func TestRemoveGroupCleansChildren(t *testing.T) {
	tree := NewTree()
	g := tree.Root().NewGroup()
	panes := make([]*Pane, 3)
	for i := 0; i < 3; i++ {
		panes[i] = emptyPane(g)
	}

	_ = tree.RemoveNode(g.Id())

	// Children must be gone from the nodes map, not just
	// unreachable from the root.
	for _, p := range panes {
		_, ok := tree.NodeById(p.Id())
		require.False(t, ok)
	}
	_, ok := tree.NodeById(g.Id())
	require.False(t, ok)
}

func TestRemoveRoot(t *testing.T) {
	tree := NewTree()
	require.Error(t, tree.RemoveNode(tree.Root().Id()))
}
