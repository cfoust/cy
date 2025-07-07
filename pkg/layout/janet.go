package layout

import (
	"fmt"

	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/layout/prop"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/style"
)

var (
	KEYWORD_PANE     = janet.Keyword("pane")
	KEYWORD_SPLIT    = janet.Keyword("split")
	KEYWORD_MARGINS  = janet.Keyword("margins")
	KEYWORD_BORDERS  = janet.Keyword("borders")
	KEYWORD_TABS     = janet.Keyword("tabs")
	KEYWORD_BAR      = janet.Keyword("bar")
	KEYWORD_COLORMAP = janet.Keyword("color-map")

	defaultBorder = prop.NewStatic(&style.DefaultBorder)
)

type nodeType struct {
	Type janet.Keyword
}

func unmarshalNode(value *janet.Value) (Node, error) {
	n := nodeType{}
	err := value.Unmarshal(&n)
	if err != nil {
		return nil, err
	}

	switch n.Type {
	case KEYWORD_COLORMAP:
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

func marshalNode(node Node) interface{} {
	switch node := node.(type) {
	case BarType:
	case ColorMapType:
	}
	return nil
}

func (l *Layout) MarshalJanet() interface{} {
	return marshalNode(l.Root)
}
