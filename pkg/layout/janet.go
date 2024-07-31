package layout

import (
	"fmt"

	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen/tree"

	"github.com/charmbracelet/lipgloss"
)

var (
	KEYWORD_PANE    = janet.Keyword("pane")
	KEYWORD_SPLIT   = janet.Keyword("split")
	KEYWORD_MARGINS = janet.Keyword("margins")

	// Border styles
	KEYWORD_NORMAL     = janet.Keyword("normal")
	KEYWORD_ROUNDED    = janet.Keyword("rounded")
	KEYWORD_BLOCK      = janet.Keyword("block")
	KEYWORD_OUTER_HALF = janet.Keyword("outer-half")
	KEYWORD_INNER_HALF = janet.Keyword("inner-half")
	KEYWORD_THICK      = janet.Keyword("thick")
	KEYWORD_DOUBLE     = janet.Keyword("double")
	KEYWORD_HIDDEN     = janet.Keyword("hidden")
	KEYWORD_NONE       = janet.Keyword("none")
)

type nodeType struct {
	Type janet.Keyword
}

func unmarshalBorder(keyword janet.Keyword) (*Border, error) {
	var border lipgloss.Border
	switch keyword {
	case KEYWORD_NORMAL:
		border = lipgloss.NormalBorder()
	case KEYWORD_ROUNDED:
		border = lipgloss.RoundedBorder()
	case KEYWORD_BLOCK:
		border = lipgloss.BlockBorder()
	case KEYWORD_OUTER_HALF:
		border = lipgloss.OuterHalfBlockBorder()
	case KEYWORD_INNER_HALF:
		border = lipgloss.InnerHalfBlockBorder()
	case KEYWORD_THICK:
		border = lipgloss.ThickBorder()
	case KEYWORD_DOUBLE:
		border = lipgloss.DoubleBorder()
	case KEYWORD_HIDDEN:
		border = lipgloss.HiddenBorder()
	case KEYWORD_NONE:
		return nil, nil
	default:
		return nil, fmt.Errorf(
			"invalid border style: %s", keyword,
		)
	}

	return &Border{
		Style:   border,
		Keyword: keyword,
	}, nil
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
			Border   *janet.Keyword
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

		if args.Border != nil {
			border, err := unmarshalBorder(*args.Border)
			if err != nil {
				return nil, err
			}
			type_.Border = border
		} else {
			type_.Border = &Border{
				Style:   lipgloss.DoubleBorder(),
				Keyword: KEYWORD_DOUBLE,
			}
		}

		if args.Vertical != nil {
			type_.Vertical = *args.Vertical
		}

		return type_, nil
	case KEYWORD_MARGINS:
		type marginsArgs struct {
			Cols   *int
			Rows   *int
			Border *janet.Keyword
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
			Node:  node,
		}

		if args.Cols != nil {
			type_.Cols = *args.Cols
		}

		if args.Rows != nil {
			type_.Rows = *args.Rows
		}

		if args.Border != nil {
			border, err := unmarshalBorder(*args.Border)
			if err != nil {
				return nil, err
			}
			type_.Border = border
		} else {
			type_.Border = &Border{
				Style:   lipgloss.DoubleBorder(),
				Keyword: KEYWORD_DOUBLE,
			}
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
			Border   *janet.Keyword
			A        interface{}
			B        interface{}
		}

		s := splitType{
			Type:     KEYWORD_SPLIT,
			Vertical: node.Vertical,
			Percent:  node.Percent,
			Cells:    node.Cells,
			A:        marshalNode(node.A),
			B:        marshalNode(node.B),
		}
		if node.Border != nil {
			s.Border = &node.Border.Keyword
		} else {
			s.Border = &KEYWORD_NONE
		}
		return s
	case MarginsType:
		return struct {
			Type  janet.Keyword
			Cols  int
			Rows  int
			Frame *string
			Node  interface{}
		}{
			Type:  KEYWORD_MARGINS,
			Cols:  node.Cols,
			Rows:  node.Rows,
			Frame: node.Frame,
			Node:  marshalNode(node.Node),
		}
	}
	return nil
}

func (l *Layout) MarshalJanet() interface{} {
	return marshalNode(l.root)
}
