package tree

import (
	"fmt"
	"sync/atomic"

	"github.com/cfoust/cy/pkg/bind"

	"github.com/sasha-s/go-deadlock"
)

type Tree struct {
	deadlock.RWMutex
	root      *Group
	nodes     map[NodeID]Node
	nodeIndex atomic.Int32
}

func (t *Tree) newMetadata() *metaData {
	t.Lock()
	defer t.Unlock()

	id := t.nodeIndex.Add(1)
	node := &metaData{
		id:    id,
		binds: bind.NewScope[Binding](),
		name:  fmt.Sprintf("%d", id),
	}

	return node
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

func NewTree() *Tree {
	tree := &Tree{
		nodes: make(map[NodeID]Node),
	}

	tree.root = &Group{
		metaData: tree.newMetadata(),
		tree:     tree,
	}

	tree.storeNode(tree.root)

	return tree
}
