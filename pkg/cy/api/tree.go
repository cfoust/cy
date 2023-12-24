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

type PaneModule struct {
	Tree *tree.Tree
}

func (p *PaneModule) Attach(context interface{}, id *janet.Value) error {
	defer id.Free()

	client, ok := context.(Client)
	if !ok {
		return fmt.Errorf("missing client context")
	}

	pane, err := resolvePane(p.Tree, id)
	if err != nil {
		return err
	}

	return client.Attach(pane)
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

func (t *TreeModule) Group(id *janet.Value) bool {
	defer id.Free()
	_, err := resolveGroup(t.Tree, id)
	return err == nil
}

func (t *TreeModule) Pane(id *janet.Value) bool {
	defer id.Free()
	_, err := resolvePane(t.Tree, id)
	return err == nil
}

func (t *TreeModule) SetName(id *janet.Value, name string) error {
	defer id.Free()

	node, err := resolveNode(t.Tree, id)
	if err != nil {
		return err
	}

	if node.Id() == t.Tree.Root().Id() {
		return fmt.Errorf("cannot rename :root")
	}

	node.SetName(name)
	return nil
}

func (t *TreeModule) Name(id *janet.Value) *string {
	defer id.Free()

	node, err := resolveNode(t.Tree, id)
	if err != nil {
		return nil
	}

	name := node.Name()
	return &name
}

func (t *TreeModule) Path(id *janet.Value) (*string, error) {
	defer id.Free()

	node, err := resolveNode(t.Tree, id)
	if err != nil {
		return nil, err
	}

	path := t.Tree.PathTo(node)
	if path == nil {
		return nil, fmt.Errorf("could not find path to node")
	}

	var result string
	for i, node := range path {
		// skip the root node
		if i == 0 {
			continue
		}

		result += fmt.Sprintf("/%s", node.Name())
	}

	return &result, nil
}

func (t *TreeModule) Parent(id *janet.Value) (*tree.NodeID, error) {
	defer id.Free()

	node, err := resolveNode(t.Tree, id)
	if err != nil {
		return nil, err
	}

	path := t.Tree.PathTo(node)
	if len(path) <= 1 {
		return nil, fmt.Errorf("node has no parent")
	}

	parentId := path[len(path)-2].Id()
	return &parentId, nil
}

func (t *TreeModule) Kill(id *janet.Value) error {
	defer id.Free()

	node, err := resolveNode(t.Tree, id)
	if err != nil {
		return err
	}

	return t.Tree.RemoveNode(node.Id())
}

func (t *TreeModule) Root() tree.NodeID {
	return t.Tree.Root().Id()
}
