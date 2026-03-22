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
		Node: &ViewNode{
			Attached: true,
		},
	})
}

// getViewNodes gets all of the views that are descendants of the provided node,
// in essence all of the leaf nodes.
func getViewNodes(tree Node) (views []*ViewNode) {
	if view, ok := tree.(*ViewNode); ok {
		return []*ViewNode{view}
	}

	for _, child := range tree.Children() {
		if child == nil {
			continue
		}

		views = append(views, getViewNodes(child)...)
	}
	return
}

// AttachFirst attaches to the first node it can find.
func AttachFirst(node Node) Node {
	node = node.Clone()
	views := getViewNodes(node)
	if len(views) == 0 {
		return node
	}

	views[0].Attached = true
	return node
}

// getNumLeaves gets the number of leaves (views) accessible from this node.
func getNumLeaves(node Node) int {
	return len(getViewNodes(node))
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

		newIndex := attached
		if newIndex >= len(newTabs) {
			newIndex = len(newTabs) - 1
		}
		newTabs[newIndex].Active = true
		newTabs[newIndex].Node = AttachFirst(newTabs[newIndex].Node)

		newNode := node
		newNode.Tabs = newTabs
		return newNode
	case *StackNode:
		leaves := node.Leaves
		if len(leaves) == 1 {
			return leaves[0].Node
		}

		var attached = -1
		for i, leaf := range leaves {
			if !leaf.Node.IsAttached() {
				continue
			}
			attached = i
			break
		}

		newLeaves := append([]Leaf{}, leaves[0:attached]...)
		if attached < len(leaves)-1 {
			newLeaves = append(
				newLeaves,
				leaves[attached+1:]...,
			)
		}

		newIndex := attached
		if newIndex >= len(newLeaves) {
			newIndex = len(newLeaves) - 1
		}
		newLeaves[newIndex].Active = true
		newLeaves[newIndex].Node = AttachFirst(newLeaves[newIndex].Node)

		newNode := node
		newNode.Leaves = newLeaves
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
	if view, ok := node.(*ViewNode); ok {
		view.Attached = false
		return view
	}

	for i, child := range node.Children() {
		node.SetChild(i, Detach(child))
	}

	return node
}

// Attach changes the currently attached tree node to the one specified by id.
func Attach(node Node, id tree.NodeID) Node {
	node = node.Clone()
	for _, view := range getViewNodes(node) {
		if !view.Attached {
			continue
		}

		view.ID = &id
		break
	}

	return node
}

// Attached returns the ID field of the attached view in the layout.
func Attached(layout Layout) *tree.NodeID {
	for _, view := range getViewNodes(layout.Root) {
		if !view.Attached {
			continue
		}

		return view.ID
	}

	return nil
}

// ValidateTree inspects a tree and ensures that it conforms to all relevant
// constraints, namely there should only be one ViewNode with Attached=true.
func ValidateTree(tree Node) error {
	numAttached := 0
	for _, view := range getViewNodes(tree) {
		if !view.Attached {
			continue
		}
		numAttached++
	}

	if numAttached > 1 {
		return fmt.Errorf("you may only attach to one view at once")
	}

	if numAttached == 0 {
		return fmt.Errorf("you must attach to at least one view")
	}

	return tree.Validate()
}
