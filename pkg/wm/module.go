package wm

import (
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
	deadlock.RWMutex
	Name string
	Data NodeData
}

func (n *Node) SetName(name string) {
	n.Lock()
	defer n.Unlock()
	n.Name = name
}

func (n *Node) GetName() string {
	n.RLock()
	name := n.Name
	n.Unlock()
	return name
}

type Group []Node

func (g Group) Type() NodeType { return NodeTypeGroup }

var _ NodeData = (*Group)(nil)

func (p *Pane) Type() NodeType { return NodeTypePane }

var _ NodeData = (*Pane)(nil)
