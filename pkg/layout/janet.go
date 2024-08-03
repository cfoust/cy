package layout

import (
	"fmt"

	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/style"

	"github.com/charmbracelet/lipgloss"
)

var (
	KEYWORD_PANE    = janet.Keyword("pane")
	KEYWORD_SPLIT   = janet.Keyword("split")
	KEYWORD_MARGINS = janet.Keyword("margins")
	KEYWORD_BORDER  = janet.Keyword("border")

	// Special border behavior
	KEYWORD_NONE = janet.Keyword("none")
)

type nodeType struct {
	Type janet.Keyword
}

func unmarshalBorder(value *janet.Value) (*style.Border, error) {
	border := style.NewBorder(
		lipgloss.RoundedBorder(),
	)

	if value == nil || value.Nil() {
		return &border, nil
	}

	if err := value.Unmarshal(&KEYWORD_NONE); err == nil {
		return nil, nil
	}

	err := value.Unmarshal(&border)
	if err != nil {
		return nil, err
	}
	return &border, nil
}

func unmarshalNode(value *janet.Value) (NodeType, error) {
	n := nodeType{}
	err := value.Unmarshal(&n)
	if err != nil {
		return nil, err
	}

	switch n.Type {
	case KEYWORD_PANE:
		type paneArgs struct {
			Attached     *bool
			RemoveOnExit *bool
			ID           *tree.NodeID
		}
		args := paneArgs{}
		err = value.Unmarshal(&args)
		if err != nil {
			return nil, err
		}
		type_ := PaneType{
			ID: args.ID,
		}

		if args.Attached != nil {
			type_.Attached = *args.Attached
		}

		if args.RemoveOnExit != nil {
			type_.RemoveOnExit = args.RemoveOnExit
		}

		return type_, err
	case KEYWORD_SPLIT:
		type splitArgs struct {
			Vertical *bool
			Percent  *int
			Cells    *int
			Border   *janet.Value
			A        *janet.Value
			B        *janet.Value
		}
		args := splitArgs{}
		err = value.Unmarshal(&args)
		if err != nil {
			return nil, err
		}

		if args.Percent != nil && args.Cells != nil {
			return nil, fmt.Errorf(
				"type :splits must have only one of :percent and :cells",
			)
		}

		a, err := unmarshalNode(args.A)
		if err != nil {
			return nil, err
		}

		b, err := unmarshalNode(args.B)
		if err != nil {
			return nil, err
		}

		type_ := SplitType{
			Percent: args.Percent,
			Cells:   args.Cells,
			A:       a,
			B:       b,
		}

		type_.Border, err = unmarshalBorder(args.Border)
		if err != nil {
			return nil, err
		}

		if args.Vertical != nil {
			type_.Vertical = *args.Vertical
		}

		return type_, nil
	case KEYWORD_MARGINS:
		type marginsArgs struct {
			Cols   *int
			Rows   *int
			Border *janet.Value
			Node   *janet.Value
		}
		args := marginsArgs{}
		err = value.Unmarshal(&args)
		if err != nil {
			return nil, err
		}

		node, err := unmarshalNode(args.Node)
		if err != nil {
			return nil, err
		}

		type_ := MarginsType{
			Node: node,
		}

		if args.Cols != nil {
			type_.Cols = *args.Cols
		}

		if args.Rows != nil {
			type_.Rows = *args.Rows
		}

		type_.Border, err = unmarshalBorder(args.Border)
		if err != nil {
			return nil, err
		}

		return type_, nil
	case KEYWORD_BORDER:
		type borderArgs struct {
			Title       *string
			TitleBottom *string
			Border      *janet.Value
			Node        *janet.Value
		}
		args := borderArgs{}
		err = value.Unmarshal(&args)
		if err != nil {
			return nil, err
		}

		node, err := unmarshalNode(args.Node)
		if err != nil {
			return nil, err
		}

		type_ := BorderType{
			Title:       args.Title,
			TitleBottom: args.TitleBottom,
			Node:        node,
		}

		type_.Border, err = unmarshalBorder(args.Border)
		if err != nil {
			return nil, err
		}

		if type_.Border == nil {
			return nil, fmt.Errorf(
				"type :border does not support :border=:none",
			)
		}

		return type_, nil
	}

	return nil, fmt.Errorf("invalid node type: %s", n.Type)
}

var _ janet.Unmarshalable = (*Layout)(nil)

func (l *Layout) UnmarshalJanet(value *janet.Value) (err error) {
	l.root, err = unmarshalNode(value)
	if err != nil {
		return
	}

	return validateTree(l.root)
}

var _ janet.Marshalable = (*Layout)(nil)

func marshalBorder(border *style.Border) interface{} {
	if border == nil {
		return KEYWORD_NONE
	}

	return border.MarshalJanet()
}

func marshalNode(node NodeType) interface{} {
	switch node := node.(type) {
	case PaneType:
		return struct {
			Type         janet.Keyword
			Attached     bool
			ID           *tree.NodeID
			RemoveOnExit *bool
		}{
			Type:         KEYWORD_PANE,
			Attached:     node.Attached,
			ID:           node.ID,
			RemoveOnExit: node.RemoveOnExit,
		}
	case SplitType:
		type splitType struct {
			Type     janet.Keyword
			Vertical bool
			Percent  *int
			Cells    *int
			Border   interface{}
			A        interface{}
			B        interface{}
		}

		s := splitType{
			Type:     KEYWORD_SPLIT,
			Vertical: node.Vertical,
			Percent:  node.Percent,
			Cells:    node.Cells,
			Border:   marshalBorder(node.Border),
			A:        marshalNode(node.A),
			B:        marshalNode(node.B),
		}
		return s
	case MarginsType:
		return struct {
			Type   janet.Keyword
			Cols   int
			Rows   int
			Frame  *string
			Border interface{}
			Node   interface{}
		}{
			Type:   KEYWORD_MARGINS,
			Cols:   node.Cols,
			Rows:   node.Rows,
			Frame:  node.Frame,
			Border: marshalBorder(node.Border),
			Node:   marshalNode(node.Node),
		}
	case BorderType:
		return struct {
			Type        janet.Keyword
			Title       *string
			TitleBottom *string
			Border      interface{}
			Node        interface{}
		}{
			Type:        KEYWORD_BORDER,
			Title:       node.Title,
			TitleBottom: node.TitleBottom,
			Border:      marshalBorder(node.Border),
			Node:        marshalNode(node.Node),
		}
	}
	return nil
}

func (l *Layout) MarshalJanet() interface{} {
	return marshalNode(l.root)
}
