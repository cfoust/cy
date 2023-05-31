package wm

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
//
// both groups and panes can have local bindings
// whenever a key is pressed, cy starts at the root node (global bindings) and
// traverses inwards down to where the user is until the key matches a binding
// or ultimately is passed as input

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
	Data   NodeData
}

type Group []Node

func (g *Group) Type() NodeType { return NodeTypeGroup }

var _ NodeData = (*Group)(nil)

func (p *Pane) Type() NodeType { return NodeTypePane }

var _ NodeData = (*Pane)(nil)
