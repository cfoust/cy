package api

import (
	"github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
)

type Client interface {
	Attach(tree.Node) error
	Node() tree.Node
	OuterLayers() *screen.Layers
}
