package api

import (
	"fmt"

	"github.com/cfoust/cy/pkg/clipboard"
	"github.com/cfoust/cy/pkg/frames"
	"github.com/cfoust/cy/pkg/layout"
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
	SetLayout(layout.Layout) error
	GetLayout() layout.Layout
	Frame() *frames.Framer
	Binds() []Binding
	Toast(toasts.Toast)
	Clipboard() clipboard.Clipboard
}

type Server interface {
	SocketName() string
	ExecuteJanet(path string) error
	Log(level zerolog.Level, message string)
	RerenderClients()
}

func getClient(context interface{}) (Client, error) {
	client, ok := context.(Client)
	if !ok {
		return nil, fmt.Errorf(
			"this functionality can only be accessed in a keybinding or action, see https://cfoust.github.io/cy/configuration.html#execution-context for more information",
		)
	}

	return client, nil
}

type Registers interface {
	Set(register string, text string) error
	Get(register string) (string, error)
	GetAll() (map[string]string, error)
}
