package wm

import (
	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/bind/trie"
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

type Callback struct {
	Docstring string
}

type BindScope = trie.Trie[Callback]

type NodeType int

const (
	NodeTypeGroup NodeType = iota
	NodeTypePane
)

type NodeData interface {
	Type() NodeType
}

type Node struct {
	Name string
	// If nil, this is a child of the root node.
	Parent *Node
	Binds  *BindScope
	Data   NodeData
}

type Group struct {
	// TODO(cfoust): 06/08/23 bindings

	children []*Node
}

func (g *Group) Type() NodeType { return NodeTypeGroup }

func (g *Group) Add(node *Node) {
	g.children = append(g.children, node)
}

func (g *Group) Children() []*Node {
	return g.children
}

func NewGroup() *Group {
	return &Group{}
}

func Wrap(name string, data NodeData) *Node {
	return &Node{
		Name:  name,
		Data:  data,
		Binds: bind.NewScope[Callback](),
	}
}

var _ NodeData = (*Group)(nil)

func (p *Pane) Type() NodeType { return NodeTypePane }

var _ NodeData = (*Pane)(nil)

// Get all of the leaf nodes (panes) of the tree.
func GetLeaves(tree *Node) (results []*Node) {
	data := tree.Data

	if data.Type() == NodeTypePane {
		results = append(results, tree)
		return
	}

	group := data.(*Group)
	for _, child := range group.Children() {
		results = append(results, GetLeaves(child)...)
	}

	return results
}
