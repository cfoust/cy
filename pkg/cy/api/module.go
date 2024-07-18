package api

import (
	"github.com/cfoust/cy/pkg/frames"
	"github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/mux/screen/toasts"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/params"

	"github.com/rs/zerolog"
)

type Client interface {
	Attach(tree.Node) error
	HistoryForward() error
	HistoryBackward() error
	Node() tree.Node
	Get(key string) (value interface{}, ok bool)
	Params() *params.Parameters
	OuterLayers() *screen.Layers
	Margins() *screen.Margins
	Frame() *frames.Framer
	Binds() []Binding
	Toast(toasts.Toast)
}

type Server interface {
	ExecuteJanet(path string) error
	Log(level zerolog.Level, message string)
}
