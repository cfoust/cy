package api

import (
	"fmt"

	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
)

type NodeParams struct {
	Name string
}

type GroupModule struct {
	Tree *tree.Tree
}

func (g *GroupModule) New(
	parentId tree.NodeID,
	params *janet.Named[NodeParams],
) (tree.NodeID, error) {
	values := params.Values()
	group, ok := g.Tree.GroupById(parentId)
	if !ok {
		return 0, fmt.Errorf("node not found: %d", parentId)
	}

	newGroup := group.NewGroup()
	if values.Name != "" {
		newGroup.SetName(values.Name)
	}

	return newGroup.Id(), nil
}

func (g *GroupModule) Children(parentId tree.NodeID) ([]tree.NodeID, error) {
	group, ok := g.Tree.GroupById(parentId)
	if !ok {
		return nil, fmt.Errorf("node not found: %d", parentId)
	}

	nodes := make([]tree.NodeID, 0)
	for _, child := range group.Children() {
		nodes = append(nodes, child.Id())
	}
	return nodes, nil
}

func (g *GroupModule) Leaves(parentId tree.NodeID) ([]tree.NodeID, error) {
	group, ok := g.Tree.GroupById(parentId)
	if !ok {
		return nil, fmt.Errorf("node not found: %d", parentId)
	}

	nodes := make([]tree.NodeID, 0)
	for _, child := range group.Leaves() {
		nodes = append(nodes, child.Id())
	}
	return nodes, nil
}

type PaneModule struct {
	Tree *tree.Tree
}

func (p *PaneModule) Attach(context interface{}, id tree.NodeID) error {
	client, ok := context.(Client)
	if !ok {
		return fmt.Errorf("missing client context")
	}

	node, ok := p.Tree.NodeById(id)
	if !ok {
		return fmt.Errorf("node not found: %d", id)
	}

	return client.Attach(node)
}

func (p *PaneModule) Current(context interface{}) *tree.NodeID {
	client, ok := context.(Client)
	if !ok {
		return nil
	}

	node := client.Node()
	if node == nil {
		return nil
	}

	id := node.Id()
	return &id
}

type TreeModule struct {
	Tree *tree.Tree
}

func (t *TreeModule) Renames() map[string]string {
	return map[string]string{
		"Group": "group?",
		"Pane":  "pane?",
	}
}

var _ janet.Renamable = (*TreeModule)(nil)

func (t *TreeModule) Group(id tree.NodeID) bool {
	_, ok := t.Tree.GroupById(id)
	return ok
}

func (t *TreeModule) Pane(id tree.NodeID) bool {
	_, ok := t.Tree.PaneById(id)
	return ok
}

func (t *TreeModule) SetName(id tree.NodeID, name string) {
	if id == t.Tree.Root().Id() {
		return
	}

	node, ok := t.Tree.NodeById(id)
	if !ok {
		return
	}

	node.SetName(name)
}

func (t *TreeModule) Name(id tree.NodeID) *string {
	node, ok := t.Tree.NodeById(id)
	if !ok {
		return nil
	}

	name := node.Name()
	return &name
}

func (t *TreeModule) Path(id tree.NodeID) *string {
	node, ok := t.Tree.NodeById(id)
	if !ok {
		return nil
	}

	path := t.Tree.PathTo(node)
	if path == nil {
		return nil
	}

	var result string
	for i, node := range path {
		// skip the root node
		if i == 0 {
			continue
		}

		result += fmt.Sprintf("/%s", node.Name())
	}

	return &result
}

func (t *TreeModule) Parent(id tree.NodeID) *tree.NodeID {
	node, ok := t.Tree.NodeById(id)
	if !ok {
		return nil
	}

	path := t.Tree.PathTo(node)
	if len(path) <= 1 {
		return nil
	}

	parentId := path[len(path)-2].Id()
	return &parentId
}

func (t *TreeModule) Kill(id tree.NodeID) error {
	return t.Tree.RemoveNode(id)
}

func (t *TreeModule) Root() tree.NodeID {
	return t.Tree.Root().Id()
}
