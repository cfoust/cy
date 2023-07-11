package wm

import (
	"context"
	"sync/atomic"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/bind/trie"
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
	*metaData
	tree     *Tree
	children []Node
}

var _ Node = (*Group)(nil)

func (g *Group) Children() []Node {
	return g.children
}

func (g *Group) addNode(node Node) {
	g.tree.storeNode(node)
	g.children = append(g.children, node)
}

func (g *Group) NewPane(
	ctx context.Context,
	options PaneOptions,
	size Size,
) *Pane {
	pane := newPane(ctx, options, size)
	pane.metaData = g.tree.newMetadata()
	g.addNode(pane)
	return pane
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
	return t.root
}

func NewTree() *Tree {
	tree := &Tree{
		nodes: make(map[NodeID]Node),
	}

	tree.root = &Group{
		metaData: tree.newMetadata(),
		tree:     tree,
	}

	return tree
}
