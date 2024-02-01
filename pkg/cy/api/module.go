package api

import (
	"github.com/cfoust/cy/pkg/frames"
	"github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/params"
)

type Client interface {
	Attach(tree.Node) error
	Node() tree.Node
	Get(key string) (value interface{}, ok bool)
	Params() *params.Parameters
	OuterLayers() *screen.Layers
	Margins() *screen.Margins
	Frame() *frames.Framer
	Binds() []Binding
}
