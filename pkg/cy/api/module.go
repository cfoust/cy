package api

import (
	"errors"

	"github.com/cfoust/cy/pkg/clipboard"
	"github.com/cfoust/cy/pkg/frames"
	"github.com/cfoust/cy/pkg/layout"
	"github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/mux/screen/toasts"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/params"

	"github.com/rs/zerolog"
)

// ErrMissingClient is returned when an API function requiring a client context
// is called outside of a keybinding or action.
var ErrMissingClient = errors.New(
	"missing client: this functionality can only be accessed in a keybinding or action, see https://cfoust.github.io/cy/configuration.html#execution-context for more information",
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

// ExecContext wraps a Client with the source node where execution originated.
// This is used when code is executed via `cy exec` to preserve the pane ID
// from which the command was invoked.
type ExecContext struct {
	Client
	SourceNode *tree.NodeID
}

// getSourceNode returns the source node from context if available.
func getSourceNode(context interface{}) *tree.NodeID {
	if exec, ok := context.(*ExecContext); ok {
		return exec.SourceNode
	}
	return nil
}

func getClient(context interface{}) (Client, error) {
	// just used by "cy exec" to resolve the pane ID correctly
	if exec, ok := context.(*ExecContext); ok {
		return exec.Client, nil
	}

	client, ok := context.(Client)
	if !ok {
		return nil, ErrMissingClient
	}

	return client, nil
}

type Registers interface {
	Set(register string, text string) error
	Get(register string) (string, error)
	GetAll() (map[string]string, error)
}
