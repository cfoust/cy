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
	KEYWORD_BORDERS = janet.Keyword("borders")
	KEYWORD_TABS    = janet.Keyword("tabs")

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

func unmarshalColor(value *janet.Value) (*style.Color, error) {
	if value == nil || value.Nil() {
		return nil, nil
	}

	var color style.Color
	err := value.Unmarshal(&color)
	if err != nil {
		return nil, err
	}
	return &color, nil
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
			BorderBg *janet.Value
			BorderFg *janet.Value
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

		type_.BorderFg, err = unmarshalColor(args.BorderFg)
		if err != nil {
			return nil, err
		}

		type_.BorderBg, err = unmarshalColor(args.BorderBg)
		if err != nil {
			return nil, err
		}

		if args.Vertical != nil {
			type_.Vertical = *args.Vertical
		}

		return type_, nil
	case KEYWORD_MARGINS:
		type marginsArgs struct {
			Cols     *int
			Rows     *int
			Border   *janet.Value
			BorderBg *janet.Value
			BorderFg *janet.Value
			Node     *janet.Value
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

		type_.BorderFg, err = unmarshalColor(args.BorderFg)
		if err != nil {
			return nil, err
		}

		type_.BorderBg, err = unmarshalColor(args.BorderBg)
		if err != nil {
			return nil, err
		}

		return type_, nil
	case KEYWORD_BORDERS:
		type borderArgs struct {
			Title       *string
			TitleBottom *string
			Border      *janet.Value
			Node        *janet.Value
			BorderBg    *janet.Value
			BorderFg    *janet.Value
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

		type_.BorderFg, err = unmarshalColor(args.BorderFg)
		if err != nil {
			return nil, err
		}

		type_.BorderBg, err = unmarshalColor(args.BorderBg)
		if err != nil {
			return nil, err
		}

		return type_, nil
	case KEYWORD_TABS:
		type tabArg struct {
			Active *bool
			Name   *string
			Node   *janet.Value
		}
		type tabsArgs struct {
			ActiveFg, ActiveBg     *janet.Value
			InactiveFg, InactiveBg *janet.Value
			Bg                     *janet.Value
			Bottom                 *bool
			Tabs                   []tabArg
		}
		args := tabsArgs{}
		err = value.Unmarshal(&args)
		if err != nil {
			return nil, err
		}

		type_ := TabsType{}

		for _, color := range []struct {
			dest **style.Color
			src  *janet.Value
		}{
			{&type_.ActiveFg, args.ActiveFg},
			{&type_.ActiveBg, args.ActiveBg},
			{&type_.InactiveFg, args.InactiveFg},
			{&type_.InactiveBg, args.InactiveBg},
		} {
			c, err := unmarshalColor(color.src)
			if err != nil {
				return nil, err
			}

			*color.dest = c
		}

		if args.Bottom != nil {
			type_.Bottom = *args.Bottom
		}

		for i, tab := range args.Tabs {
			newTab := Tab{}
			if tab.Active != nil {
				newTab.Active = *tab.Active
			}

			if tab.Name == nil || len(*tab.Name) == 0 {
				return nil, fmt.Errorf("tab %d has empty name", i)
			}

			newTab.Name = *tab.Name

			node, err := unmarshalNode(tab.Node)
			if err != nil {
				return nil, fmt.Errorf("tab %d invalid: %s", i, err)
			}

			newTab.Node = node

			type_.Tabs = append(type_.Tabs, newTab)
		}

		return type_, nil
	}

	return nil, fmt.Errorf("invalid node type: %s", n.Type)
}

var _ janet.Unmarshalable = (*Layout)(nil)

func (l *Layout) UnmarshalJanet(value *janet.Value) (err error) {
	l.Root, err = unmarshalNode(value)
	if err != nil {
		return
	}

	return ValidateTree(l.Root)
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
			BorderFg *style.Color
			BorderBg *style.Color
			A        interface{}
			B        interface{}
		}

		s := splitType{
			Type:     KEYWORD_SPLIT,
			Vertical: node.Vertical,
			Percent:  node.Percent,
			Cells:    node.Cells,
			Border:   marshalBorder(node.Border),
			BorderFg: node.BorderFg,
			BorderBg: node.BorderBg,
			A:        marshalNode(node.A),
			B:        marshalNode(node.B),
		}
		return s
	case MarginsType:
		return struct {
			Type     janet.Keyword
			Cols     int
			Rows     int
			Frame    *string
			Border   interface{}
			BorderFg *style.Color
			BorderBg *style.Color
			Node     interface{}
		}{
			Type:     KEYWORD_MARGINS,
			Cols:     node.Cols,
			Rows:     node.Rows,
			Frame:    node.Frame,
			Border:   marshalBorder(node.Border),
			BorderFg: node.BorderFg,
			BorderBg: node.BorderBg,
			Node:     marshalNode(node.Node),
		}
	case BorderType:
		return struct {
			Type        janet.Keyword
			Title       *string
			TitleBottom *string
			Border      interface{}
			BorderFg    *style.Color
			BorderBg    *style.Color
			Node        interface{}
		}{
			Type:        KEYWORD_BORDERS,
			Title:       node.Title,
			TitleBottom: node.TitleBottom,
			Border:      marshalBorder(node.Border),
			BorderFg:    node.BorderFg,
			BorderBg:    node.BorderBg,
			Node:        marshalNode(node.Node),
		}
	case TabsType:
		type tabArg struct {
			Active bool
			Name   string
			Node   interface{}
		}
		type_ := struct {
			Type                   janet.Keyword
			ActiveFg, ActiveBg     *style.Color
			InactiveFg, InactiveBg *style.Color
			Bg                     *style.Color
			Bottom                 bool
			Tabs                   []tabArg
		}{
			Type:       KEYWORD_TABS,
			ActiveFg:   node.ActiveFg,
			ActiveBg:   node.ActiveBg,
			InactiveFg: node.InactiveFg,
			InactiveBg: node.InactiveBg,
			Bg:         node.Bg,
			Bottom:     node.Bottom,
		}

		for _, tab := range node.Tabs {
			type_.Tabs = append(
				type_.Tabs,
				tabArg{
					Active: tab.Active,
					Name:   tab.Name,
					Node:   marshalNode(tab.Node),
				},
			)
		}

		return type_
	}
	return nil
}

func (l *Layout) MarshalJanet() interface{} {
	return marshalNode(l.Root)
}
