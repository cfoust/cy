package layout

import (
	"fmt"

	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/layout/prop"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
)

var (
	KEYWORD_PANE    = janet.Keyword("pane")
	KEYWORD_SPLIT   = janet.Keyword("split")
	KEYWORD_MARGINS = janet.Keyword("margins")
	KEYWORD_BORDERS = janet.Keyword("borders")
	KEYWORD_TABS    = janet.Keyword("tabs")
	KEYWORD_BAR     = janet.Keyword("bar")
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
			A        *janet.Value
			B        *janet.Value
			Border   *prop.Border
			BorderFg *prop.Color
			BorderBg *prop.Color
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
			Percent:  args.Percent,
			Cells:    args.Cells,
			A:        a,
			B:        b,
			Border:   args.Border,
			BorderFg: args.BorderFg,
			BorderBg: args.BorderBg,
		}

		if args.Vertical != nil {
			type_.Vertical = *args.Vertical
		}

		return type_, nil
	case KEYWORD_MARGINS:
		type marginsArgs struct {
			Cols     *int
			Rows     *int
			Border   *prop.Border
			BorderBg *prop.Color
			BorderFg *prop.Color
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
			Node:     node,
			Border:   args.Border,
			BorderFg: args.BorderFg,
			BorderBg: args.BorderBg,
		}

		if args.Cols != nil {
			type_.Cols = *args.Cols
		}

		if args.Rows != nil {
			type_.Rows = *args.Rows
		}

		return type_, nil
	case KEYWORD_BORDERS:
		type borderArgs struct {
			Title       *prop.String
			TitleBottom *prop.String
			Border      *prop.Border
			Node        *janet.Value
			BorderBg    *prop.Color
			BorderFg    *prop.Color
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
			Border:      args.Border,
			BorderFg:    args.BorderFg,
		}

		if type_.Border != nil {
			if border, ok := type_.Border.Static(); ok && border.None() {
				return nil, fmt.Errorf(
					"type :border does not support :border=:none",
				)
			}
		}

		return type_, nil
	case KEYWORD_TABS:
		type tabArg struct {
			Active *bool
			Name   *string
			Node   *janet.Value
		}
		type tabsArgs struct {
			ActiveFg, ActiveBg     *prop.Color
			InactiveFg, InactiveBg *prop.Color
			Bg                     *prop.Color
			Bottom                 *bool
			Tabs                   []tabArg
		}
		args := tabsArgs{}
		err = value.Unmarshal(&args)
		if err != nil {
			return nil, err
		}

		type_ := TabsType{
			ActiveFg:   args.ActiveFg,
			ActiveBg:   args.ActiveBg,
			InactiveFg: args.InactiveFg,
			InactiveBg: args.InactiveBg,
			Bg:         args.Bg,
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
	case KEYWORD_BAR:
		type barArgs struct {
			Text   *prop.String
			Bottom *bool
			Node   *janet.Value
		}
		args := barArgs{}
		err = value.Unmarshal(&args)
		if err != nil {
			return nil, err
		}

		node, err := unmarshalNode(args.Node)
		if err != nil {
			return nil, err
		}

		if args.Text == nil {
			return nil, fmt.Errorf(
				":bar node missing :text",
			)
		}

		type_ := BarType{
			Node: node,
			Text: args.Text,
		}

		if args.Bottom != nil {
			type_.Bottom = *args.Bottom
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
			Border   *prop.Border
			BorderFg *prop.Color
			BorderBg *prop.Color
			A        interface{}
			B        interface{}
		}

		s := splitType{
			Type:     KEYWORD_SPLIT,
			Vertical: node.Vertical,
			Percent:  node.Percent,
			Cells:    node.Cells,
			Border:   node.Border,
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
			Border   *prop.Border
			BorderFg *prop.Color
			BorderBg *prop.Color
			Node     interface{}
		}{
			Type:     KEYWORD_MARGINS,
			Cols:     node.Cols,
			Rows:     node.Rows,
			Frame:    node.Frame,
			Border:   node.Border,
			BorderFg: node.BorderFg,
			BorderBg: node.BorderBg,
			Node:     marshalNode(node.Node),
		}
	case BorderType:
		return struct {
			Type        janet.Keyword
			Title       *prop.String
			TitleBottom *prop.String
			Border      *prop.Border
			BorderFg    *prop.Color
			BorderBg    *prop.Color
			Node        interface{}
		}{
			Type:        KEYWORD_BORDERS,
			Title:       node.Title,
			TitleBottom: node.TitleBottom,
			Border:      node.Border,
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
			ActiveFg, ActiveBg     *prop.Color
			InactiveFg, InactiveBg *prop.Color
			Bg                     *prop.Color
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
	case BarType:
		return struct {
			Type   janet.Keyword
			Text   *prop.String
			Bottom bool
			Node   interface{}
		}{
			Type:   KEYWORD_BAR,
			Text:   node.Text,
			Bottom: node.Bottom,
			Node:   marshalNode(node.Node),
		}
	}
	return nil
}

func (l *Layout) MarshalJanet() interface{} {
	return marshalNode(l.Root)
}
