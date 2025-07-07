package layout

import (
	"context"
	"errors"
	"fmt"

	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/layout/prop"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/params"
	"github.com/cfoust/cy/pkg/style"
)

var ErrChildNil = errors.New("child nodes cannot be nil")

type NodeType int

type Node interface {
	Type() NodeType
	IsAttached() bool
	Children() []Node
	SetChild(index int, node Node)
	VisibleChildren() []Node
	SetVisibleChild(index int, node Node)
	Clone() Node
	Validate() error
	MarshalJanet() interface{}
	UnmarshalJanet(*janet.Value) (Node, error)
	// Create a Screen from this Node.
	Screen(
		ctx context.Context,
		tree *tree.Tree,
		server *server.Server,
		params *params.Parameters,
		children []Reusable,
	) Reusable
}

type Layout struct {
	Root Node
}

func New(node Node) Layout {
	return Layout{Root: node}
}

// Default returns the default layout.
func Default() Layout {
	return New(&MarginsNode{
		Cols:   80,
		Border: prop.NewStatic(&style.DefaultBorder),
		Node: &PaneNode{
			Attached: true,
		},
	})
}

// getPaneNodes gets all of the panes that are descendants of the provided node,
// in essence all of the leaf nodes.
func getPaneNodes(tree Node) (panes []*PaneNode) {
	if pane, ok := tree.(*PaneNode); ok {
		return []*PaneNode{pane}
	}

	for _, child := range tree.Children() {
		if child == nil {
			continue
		}

		panes = append(panes, getPaneNodes(child)...)
	}
	return
}

// AttachFirst attaches to the first node it can find.
func AttachFirst(node Node) Node {
	panes := getPaneNodes(node)
	if len(panes) == 0 {
		return node
	}

	node = node.Clone()
	panes[0].Attached = true
	return node
}

// getNumLeaves gets the number of leaves (panes) accessible from this node.
func getNumLeaves(node Node) int {
	return len(getPaneNodes(node))
}

// RemoveAttached removes the attached node by replacing its nearest parent
// that has more than one child with a parent with that child removed, or the
// other child if there are no other children.
func RemoveAttached(node Node) Node {
	if !node.IsAttached() {
		return node
	}

	switch node := node.(type) {
	case *SplitNode:
		if node.A.IsAttached() && getNumLeaves(node.A) == 1 {
			return AttachFirst(node.B)
		}

		if node.B.IsAttached() && getNumLeaves(node.B) == 1 {
			return AttachFirst(node.A)
		}

		node.A = RemoveAttached(node.A)
		node.B = RemoveAttached(node.B)
		return node
	case *TabsNode:
		tabs := node.Tabs
		if len(tabs) == 1 {
			return tabs[0].Node
		}

		var attached = -1
		for i, tab := range tabs {
			if !tab.Node.IsAttached() {
				continue
			}
			attached = i
			break
		}

		newTabs := append([]Tab{}, tabs[0:attached]...)
		if attached < len(tabs)-1 {
			newTabs = append(newTabs, tabs[attached+1:]...)
		}

		newTabs[0].Active = true
		newTabs[0].Node = AttachFirst(newTabs[0].Node)

		newNode := node
		newNode.Tabs = newTabs
		return newNode
	default:
		for i, child := range node.Children() {
			node.SetChild(i, RemoveAttached(child))
		}
		return node
	}
}

// Detach returns a copy of `node` with no attachment points.
func Detach(node Node) Node {
	// TODO(cfoust): 07/07/25 this creates a lot of extra copies
	node = node.Clone()
	if pane, ok := node.(*PaneNode); ok {
		pane.Attached = false
		return pane
	}

	for i, child := range node.Children() {
		node.SetChild(i, Detach(child))
	}

	return node
}

// attach returns a copy of node with the NodeID of the current attachment
// point replaced with id.
func attach(node Node, id tree.NodeID) Node {
	node = node.Clone()
	if pane, ok := node.(*PaneNode); ok {
		if pane.Attached {
			pane.ID = &id
		}

		return pane
	}

	for _, child := range node.Children() {
		attach(child, id)
	}

	return node
}

// Attach changes the currently attached tree node to the one specified by id.
func Attach(layout Layout, id tree.NodeID) Layout {
	return Layout{Root: attach(layout.Root, id)}
}

// Attached returns the ID field of the attached pane in the layout.
func Attached(layout Layout) *tree.NodeID {
	for _, pane := range getPaneNodes(layout.Root) {
		if !pane.Attached {
			continue
		}

		return pane.ID
	}

	return nil
}

// ValidateTree inspects a tree and ensures that it conforms to all relevant
// constraints, namely there should only be one PaneType with Attached=true.
func ValidateTree(tree Node) error {
	numAttached := 0
	for _, pane := range getPaneNodes(tree) {
		if pane.Attached != true {
			continue
		}
		numAttached++
	}

	if numAttached > 1 {
		return fmt.Errorf("you may only attach to one pane at once")
	}

	if numAttached == 0 {
		return fmt.Errorf("you must attach to at least one pane")
	}

	return tree.Validate()
}
