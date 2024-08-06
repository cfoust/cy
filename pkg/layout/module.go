package layout

import (
	"fmt"

	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/style"
)

type NodeType interface{}

type PaneType struct {
	Attached     bool
	RemoveOnExit *bool
	ID           *tree.NodeID
}

type SplitType struct {
	Vertical bool
	Percent  *int
	Cells    *int
	Border   *style.Border
	A        NodeType
	B        NodeType
}

type MarginsType struct {
	Cols   int
	Rows   int
	Frame  *string
	Border *style.Border
	Node   NodeType
}

type BorderType struct {
	Title       *string
	TitleBottom *string
	Border      *style.Border
	Node        NodeType
}

type Layout struct {
	Root NodeType
}

func New(node NodeType) Layout {
	return Layout{Root: node}
}

type NodeChangeEvent struct {
	Config NodeType
}

type NodeRemoveEvent struct{}

// getPaneType gets all of the panes that are descendants of the provided node,
// in essence all of the leaf nodes.
func getPaneType(tree NodeType) (panes []PaneType) {
	switch node := tree.(type) {
	case PaneType:
		return []PaneType{node}
	case SplitType:
		panes = append(panes, getPaneType(node.A)...)
		panes = append(panes, getPaneType(node.B)...)
		return
	case MarginsType:
		panes = append(panes, getPaneType(node.Node)...)
		return
	case BorderType:
		panes = append(panes, getPaneType(node.Node)...)
		return
	}
	return
}

// getNumLeaves gets the number of leaves (panes) accessible from this node.
func getNumLeaves(node NodeType) int {
	switch node := node.(type) {
	case PaneType:
		return 1
	case SplitType:
		return getNumLeaves(node.A) + getNumLeaves(node.B)
	case MarginsType:
		return getNumLeaves(node.Node)
	case BorderType:
		return getNumLeaves(node.Node)
	}
	return 0
}

// AttachFirst attaches to the first node it can find.
func AttachFirst(node NodeType) NodeType {
	switch node := node.(type) {
	case PaneType:
		node.Attached = true
		return node
	case SplitType:
		node.A = AttachFirst(node.A)
		return node
	case MarginsType:
		node.Node = AttachFirst(node.Node)
		return node
	case BorderType:
		node.Node = AttachFirst(node.Node)
		return node
	}

	return node
}

// RemoveAttached removes the attached node by replacing its nearest parent
// that has more than one child with a parent with that child removed, or the
// other child if there are no other children.
func RemoveAttached(node NodeType) NodeType {
	if !IsAttached(node) {
		return node
	}

	switch node := node.(type) {
	case PaneType:
		return node
	case SplitType:
		if IsAttached(node.A) && getNumLeaves(node.A) == 1 {
			return AttachFirst(node.B)
		}

		if IsAttached(node.B) && getNumLeaves(node.B) == 1 {
			return AttachFirst(node.A)
		}

		node.A = RemoveAttached(node.A)
		node.B = RemoveAttached(node.B)
		return node
	case MarginsType:
		node.Node = RemoveAttached(node.Node)
		return node
	case BorderType:
		node.Node = RemoveAttached(node.Node)
		return node
	}

	return node
}

// IsAttached reports whether the node provided leads to a node that is
// attached.
func IsAttached(tree NodeType) bool {
	switch node := tree.(type) {
	case PaneType:
		return node.Attached
	case SplitType:
		return IsAttached(node.A) || IsAttached(node.B)
	case MarginsType:
		return IsAttached(node.Node)
	case BorderType:
		return IsAttached(node.Node)
	}
	return false
}

// Detach returns a copy of node with no attachment points.
func Detach(node NodeType) NodeType {
	switch node := node.(type) {
	case PaneType:
		node.Attached = false
		return node
	case SplitType:
		node.A = Detach(node.A)
		node.B = Detach(node.B)
		return node
	case MarginsType:
		node.Node = Detach(node.Node)
		return node
	case BorderType:
		node.Node = Detach(node.Node)
		return node
	}

	return node
}

// attach returns a copy of node with the NodeID of the current attachment
// point replaced with id.
func attach(node NodeType, id tree.NodeID) NodeType {
	switch node := node.(type) {
	case PaneType:
		if node.Attached {
			return PaneType{
				Attached: true,
				ID:       &id,
			}
		}

		return node
	case SplitType:
		node.A = attach(node.A, id)
		node.B = attach(node.B, id)
		return node
	case MarginsType:
		node.Node = attach(node.Node, id)
		return node
	case BorderType:
		node.Node = attach(node.Node, id)
		return node
	}

	return node
}

// Attach changes the currently attached tree node to the one specified by id.
func Attach(layout Layout, id tree.NodeID) Layout {
	return Layout{Root: attach(layout.Root, id)}
}

func getAttached(node NodeType) *tree.NodeID {
	switch node := node.(type) {
	case PaneType:
		if node.Attached {
			return node.ID
		}

		return nil
	case SplitType:
		if idA := getAttached(node.A); idA != nil {
			return idA
		}
		if idB := getAttached(node.B); idB != nil {
			return idB
		}
		return nil
	case MarginsType:
		return getAttached(node.Node)
	case BorderType:
		return getAttached(node.Node)
	}

	return nil
}

// Attached returns the ID field of the attached pane in the layout.
func Attached(layout Layout) *tree.NodeID {
	return getAttached(layout.Root)
}

// ValidateTree inspects a tree and ensures that it conforms to all relevant
// constraints, namely there should only be one PaneType with Attached=true.
func ValidateTree(tree NodeType) error {
	numAttached := 0
	for _, pane := range getPaneType(tree) {
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

	return nil
}

// Reusable is used to describe a Screen that has a configuration that can
// change. Often a Screen does not actually need to be created from scratch
// when the corresponding layout node changes; it can just be updated. The
// reuse function checks whether the Screen can be updated to match the new
// configuration and updates it if possible.
type Reusable interface {
	mux.Screen
	Apply(NodeType) (bool, error)
}
