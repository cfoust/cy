package layout

import (
	"fmt"

	"github.com/cfoust/cy/pkg/layout/prop"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/style"
)

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

// getPaneType gets all of the panes that are descendants of the provided node,
// in essence all of the leaf nodes.
func getPaneType(tree NodeType) (panes []PaneType) {
	switch node := tree.(type) {
	case PaneType:
		return []PaneType{node}
	case SplitType:
		panes = append(panes, getPaneType(node.A)...)
		panes = append(panes, getPaneType(node.B)...)
	case MarginsType:
		panes = append(panes, getPaneType(node.Node)...)
	case BorderType:
		panes = append(panes, getPaneType(node.Node)...)
	case BarType:
		panes = append(panes, getPaneType(node.Node)...)
	case TabsType:
		for _, tab := range node.Tabs {
			panes = append(panes, getPaneType(tab.Node)...)
		}
	case ColorMapType:
		panes = append(panes, getPaneType(node.Node)...)
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
	case BarType:
		return getNumLeaves(node.Node)
	case TabsType:
		var result int
		for _, tab := range node.Tabs {
			result += getNumLeaves(tab.Node)
		}
		return result
	case ColorMapType:
		return getNumLeaves(node.Node)
	}
	return 0
}

// AttachFirst attaches to the first node it can find.
func AttachFirst(node Node) Node {
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
	case BarType:
		node.Node = AttachFirst(node.Node)
		return node
	case TabsType:
		for i, tab := range node.Tabs {
			if !tab.Active {
				continue
			}
			node.Tabs[i].Node = AttachFirst(tab.Node)
		}
		return node
	case ColorMapType:
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
	case BarType:
		node.Node = RemoveAttached(node.Node)
		return node
	case TabsType:
		tabs := node.Tabs
		if len(tabs) == 1 {
			return tabs[0].Node
		}

		var attached = -1
		for i, tab := range tabs {
			if !IsAttached(tab.Node) {
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
	case ColorMapType:
		node.Node = RemoveAttached(node.Node)
		return node
	}

	return node
}

// Detach returns a copy of node with no attachment points.
func Detach(node Node) Node {
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
	case BarType:
		node.Node = Detach(node.Node)
		return node
	case TabsType:
		for i, tab := range node.Tabs {
			node.Tabs[i].Node = Detach(tab.Node)
		}
		return node
	case ColorMapType:
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
	case BarType:
		node.Node = attach(node.Node, id)
		return node
	case TabsType:
		for i, tab := range node.Tabs {
			node.Tabs[i].Node = attach(tab.Node, id)
		}
		return node
	case ColorMapType:
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
	case BarType:
		return getAttached(node.Node)
	case TabsType:
		for _, tab := range node.Tabs {
			if IsAttached(tab.Node) {
				return getAttached(tab.Node)
			}
		}
		return nil
	case ColorMapType:
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

	return validateNodes(tree)
}
