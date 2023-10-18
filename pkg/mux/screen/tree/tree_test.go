package tree

import (
	"context"
	"testing"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux/stream"

	"github.com/stretchr/testify/require"
)

func TestRoot(t *testing.T) {
	tree := NewTree(nil, nil)
	require.Equal(t, NodeID(1), tree.Root().Id())
	require.Equal(t, 0, len(tree.Leaves()))
}

func emptyPane(g *Group) *Pane {
	return g.NewPane(
		context.Background(),
		stream.NewReader(),
		geom.DEFAULT_SIZE,
	)
}

func TestLeaves(t *testing.T) {
	tree := NewTree(nil, nil)
	g := tree.Root().NewGroup()
	for i := 0; i < 3; i++ {
		emptyPane(g)
	}
	require.Equal(t, 3, len(g.Children()))
	require.Equal(t, 3, len(tree.Leaves()))
}

func TestRemoveGroup(t *testing.T) {
	tree := NewTree(nil, nil)
	g := tree.Root().NewGroup()
	for i := 0; i < 3; i++ {
		emptyPane(g)
	}

	tree.RemoveNode(g.Id())
	require.Equal(t, 0, len(tree.Leaves()))
}

func TestRemoveNode(t *testing.T) {
	tree := NewTree(nil, nil)
	g := tree.Root().NewGroup()
	for i := 0; i < 3; i++ {
		emptyPane(g)
	}

	child := g.Children()[1]
	tree.RemoveNode(child.Id())
	require.Equal(t, 2, len(tree.Leaves()))
	require.Equal(t, 2, len(g.Children()))
}

func TestRemoveRoot(t *testing.T) {
	tree := NewTree(nil, nil)
	require.Error(t, tree.RemoveNode(tree.Root().Id()))
}
