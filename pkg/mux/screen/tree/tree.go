package tree

import (
	"fmt"
	"sync/atomic"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/params"

	"github.com/sasha-s/go-deadlock"
)

type Tree struct {
	deadlock.RWMutex
	*mux.UpdatePublisher
	root       *Group
	nodes      map[NodeID]Node
	nextNodeID atomic.Int32
}

func (t *Tree) newMetadata(node Node) *metaData {
	t.Lock()
	defer t.Unlock()

	id := t.nextNodeID.Add(1)
	metadata := &metaData{
		id:    id,
		binds: bind.NewBindScope(node),
		name:  fmt.Sprintf("%d", id),
	}

	return metadata
}

func (t *Tree) storeNode(node Node) {
	t.Lock()
	defer t.Unlock()

	t.nodes[node.Id()] = node
}

func (t *Tree) Root() *Group {
	t.RLock()
	defer t.RUnlock()

	return t.root
}

// Get the path from the root node to the given node.
func (t *Tree) PathTo(node Node) []Node {
	t.RLock()
	defer t.RUnlock()

	return findPath(t.root, node)
}

func (t *Tree) Leaves() []Node {
	t.RLock()
	defer t.RUnlock()

	return getLeaves(t.root)
}

func (t *Tree) NodeById(id NodeID) (Node, bool) {
	t.RLock()
	defer t.RUnlock()

	node, ok := t.nodes[id]
	return node, ok
}

func (t *Tree) RemoveNode(id NodeID) error {
	if t.root.Id() == id {
		return fmt.Errorf("cannot remove root node")
	}

	node, ok := t.NodeById(id)
	if !ok {
		return fmt.Errorf("node %d does not exist", id)
	}

	path := t.PathTo(node)
	if len(path) == 0 {
		return nil
	}

	parent := path[len(path)-2]
	parent.(*Group).removeNode(node)

	switch node := node.(type) {
	case *Pane:
		t.Lock()
		node.Cancel()
		delete(t.nodes, id)
		t.Unlock()
	case *Group:
		t.Lock()
		delete(t.nodes, id)
		t.Unlock()

		for _, child := range node.children {
			t.RemoveNode(child.Id())
		}
	}

	return nil
}

func (t *Tree) PaneById(id NodeID) (*Pane, bool) {
	t.RLock()
	defer t.RUnlock()

	node, ok := t.nodes[id]
	if !ok {
		return nil, false
	}

	pane, ok := node.(*Pane)
	if !ok {
		return nil, false
	}

	return pane, true
}

func (t *Tree) GroupById(id NodeID) (*Group, bool) {
	t.RLock()
	defer t.RUnlock()

	node, ok := t.nodes[id]
	if !ok {
		return nil, false
	}

	group, ok := node.(*Group)
	if !ok {
		return nil, false
	}

	return group, true
}

type TreeOption func(*Tree)

func WithParams(p *params.Parameters) TreeOption {
	return func(t *Tree) {
		t.root.params = p
	}
}

func NewTree(options ...TreeOption) *Tree {
	tree := &Tree{
		UpdatePublisher: mux.NewPublisher(),
		nodes:           make(map[NodeID]Node),
	}

	root := &Group{tree: tree}
	root.metaData = tree.newMetadata(root)
	tree.root = root
	tree.storeNode(tree.root)

	for _, option := range options {
		option(tree)
	}

	return tree
}
