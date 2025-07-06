package layout

import (
	"fmt"

	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/layout/prop"
	"github.com/cfoust/cy/pkg/style"
)

var defaultBorder = prop.NewStatic(&style.DefaultBorder)

type nodeType struct {
	Type janet.Keyword
}

func unmarshalNode(value *janet.Value) (Node, error) {
	n := nodeType{}
	for _type, node := range janetTypes {
		n.Type = janet.Keyword(_type)
		err := value.Unmarshal(&n)
		if err != nil {
			continue
		}

		unmarshaled, err := node.UnmarshalJanet(value)
		if err != nil {
			return nil, fmt.Errorf(
				"could not unmarshal :%s: %w",
				_type,
				err,
			)
		}

		return unmarshaled, nil
	}

	// Reset this to empty so it's not compared
	n.Type = ""
	err := value.Unmarshal(&n)
	if err != nil {
		// :type was not present
		return nil, err
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

func (l *Layout) MarshalJanet() interface{} {
	return l.Root.MarshalJanet()
}
