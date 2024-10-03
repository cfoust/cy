package api

import (
	"fmt"

	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
)

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

func (t *TreeModule) Rm(id *janet.Value) error {
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
