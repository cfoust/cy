package layout

import (
	"fmt"

	"github.com/cfoust/cy/pkg/janet"
)

var (
	KEYWORD_PANE    = janet.Keyword("pane")
	KEYWORD_SPLIT   = janet.Keyword("splits")
	KEYWORD_MARGINS = janet.Keyword("margins")
)

type nodeType struct {
	Type janet.Keyword
}

func unmarshalNode(value *janet.Value) (NodeType, error) {
	n := nodeType{}
	err := value.Unmarshal(&n)
	if err != nil {
		return nil, err
	}

	switch n.Type {
	case KEYWORD_PANE:
		type_ := PaneType{}
		err = value.Unmarshal(&type_)
		return type_, err
	case KEYWORD_SPLIT:
		type splitArgs struct {
			Vertical *bool
			Percent  *int
			Cells    *int
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

		if args.Vertical != nil {
			type_.Vertical = *args.Vertical
		}

		return type_, nil
	case KEYWORD_MARGINS:
		type marginsArgs struct {
			Cols  *int
			Rows  *int
			Frame *string
			Node  *janet.Value
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
			Frame: args.Frame,
			Node:  node,
		}

		if args.Cols != nil {
			type_.Cols = *args.Cols
		}

		if args.Rows != nil {
			type_.Rows = *args.Rows
		}

		return type_, nil
	}

	return nil, fmt.Errorf("invalid node type: %s", n.Type)
}

var _ janet.Unmarshalable = (*LayoutType)(nil)

func (l *LayoutType) UnmarshalJanet(value *janet.Value) (err error) {
	l.Root, err = unmarshalNode(value)
	return err
}
