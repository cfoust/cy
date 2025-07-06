package layout

import (
	"github.com/cfoust/cy/pkg/janet"
)

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

type JanetIO interface {
	Type() string
}
