package api

import (
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
)

type GroupModule struct {
	Tree *tree.Tree
}

type NodeParams struct {
	Name string
}

func (g *GroupModule) New(
	parentId *janet.Value,
	params *janet.Named[NodeParams],
) (tree.NodeID, error) {
	defer parentId.Free()
	values := params.Values()

	group, err := resolveGroup(g.Tree, parentId)
	if err != nil {
		return 0, err
	}

	newGroup := group.NewGroup()
	if values.Name != "" {
		newGroup.SetName(values.Name)
	}

	return newGroup.Id(), nil
}

func (g *GroupModule) Children(parentId *janet.Value) ([]tree.NodeID, error) {
	defer parentId.Free()

	group, err := resolveGroup(g.Tree, parentId)
	if err != nil {
		return nil, err
	}

	nodes := make([]tree.NodeID, 0)
	for _, child := range group.Children() {
		nodes = append(nodes, child.Id())
	}
	return nodes, nil
}

func (g *GroupModule) Leaves(parentId *janet.Value) ([]tree.NodeID, error) {
	defer parentId.Free()

	group, err := resolveGroup(g.Tree, parentId)
	if err != nil {
		return nil, err
	}

	nodes := make([]tree.NodeID, 0)
	for _, child := range group.Leaves() {
		nodes = append(nodes, child.Id())
	}

	return nodes, nil
}
