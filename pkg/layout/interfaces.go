package layout

import (
	"errors"

	"github.com/cfoust/cy/pkg/janet"
)

var ErrChildNil = errors.New("child nodes cannot be nil")

type NodeType int

type Node interface {
	Type() NodeType
	IsAttached() bool
	Children() []Node
	SetChild(index int, node Node)
	Clone() Node
	Validate() error
	MarshalJanet() interface{}
	UnmarshalJanet(*janet.Value) (Node, error)
}
