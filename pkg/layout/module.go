package layout

import (
	"github.com/cfoust/cy/pkg/mux/screen/tree"
)

type NodeType interface{}

type PaneType struct {
	Attached *bool
	ID       *tree.NodeID
}

type SplitType struct {
	Vertical bool
	Percent  *int
	Cells    *int
	A        NodeType
	B        NodeType
}

type MarginsType struct {
	Cols  int
	Rows  int
	Frame *string
	Node  NodeType
}

type LayoutType struct {
	Root NodeType
}
