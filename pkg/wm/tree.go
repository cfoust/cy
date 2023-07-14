package wm

import (
	"context"
	"sync/atomic"

	"github.com/cfoust/cy/pkg/app"
	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/bind/trie"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/janet"

	"github.com/sasha-s/go-deadlock"
)

// cy uses a tree to represent panes and groups of them:
// projects
// - cy
//   - editor
//   - shell
// - sour
//   - editor
//   - shell
// shells
// - shell
// - shell
// - shell

type Binding struct {
	Description string
	Callback    *janet.Function
}

type BindScope = trie.Trie[Binding]

type NodeID = int32

type metaData struct {
	id    NodeID
	name  string
	binds *BindScope
}

func (m *metaData) Id() int32 {
	return m.id
}

func (m *metaData) Name() string {
	return m.name
}

func (m *metaData) SetName(name string) {
	m.name = name
}

func (m *metaData) Binds() *BindScope {
	return m.binds
}

type Node interface {
	Id() NodeID
	Name() string
	SetName(string)
	Binds() *BindScope
}

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
	app app.IO,
	size geom.Size,
) *Pane {
	pane := newPane(ctx, app, size)
	pane.metaData = g.tree.newMetadata()
	g.addNode(pane)
	return pane
}

func (g *Group) NewCmd(ctx context.Context, options app.CmdOptions, size geom.Size) (*Pane, error) {
	cmd, err := app.NewCmd(ctx, options, size)
	if err != nil {
		return nil, err
	}
	pane := newPane(ctx, cmd, size)
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

type Tree struct {
	deadlock.RWMutex
	root      *Group
	nodes     map[NodeID]Node
	nodeIndex atomic.Int32
}

func (t *Tree) newMetadata() *metaData {
	t.Lock()
	defer t.Unlock()

	node := &metaData{
		id:    t.nodeIndex.Add(1),
		binds: bind.NewScope[Binding](),
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

func findPath(current, needle Node) (result []Node) {
	if current == needle {
		result = append(result, current)
		return
	}

	switch node := current.(type) {
	case *Pane:
		return
	case *Group:
		for _, child := range node.Children() {
			path := findPath(child, needle)
			if len(path) == 0 {
				continue
			}

			result = append(
				[]Node{current},
				path...,
			)
			return
		}
	}

	return
}

// Get the path from the root node to the given node.
func (t *Tree) PathTo(node Node) []Node {
	t.RLock()
	defer t.RUnlock()

	return findPath(t.root, node)
}

func getLeaves(node Node) (result []Node) {
	switch node := node.(type) {
	case *Pane:
		result = append(result, node)
		return
	case *Group:
		for _, child := range node.Children() {
			result = append(
				result,
				getLeaves(child)...,
			)
			return
		}
	}

	return
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
