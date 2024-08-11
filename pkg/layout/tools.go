package layout

import (
	"context"

	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/util"

	"github.com/rs/zerolog"
)

// Reusable is used to describe a Screen that has a configuration that can
// change. Often a Screen does not actually need to be created from scratch
// when the corresponding layout node changes; it can just be updated. The
// reuse function checks whether the Screen can be updated to match the new
// configuration and updates it if possible.
type Reusable interface {
	mux.Screen
	Apply(NodeType) (bool, error)
}

// Loggable is used for passing a logger to the Screen when appropriate.
type Loggable interface {
	SetLogger(zerolog.Logger)
}

type Log struct {
	zerolog.Logger
}

func NewLog() *Log {
	return &Log{}
}

var _ Loggable = (*Log)(nil)

func (l *Log) SetLogger(log zerolog.Logger) {
	l.Logger = log
}

// Contextable is used for passing in an execution context that will be used
// when running Janet functions.
type Contextable interface {
	SetContext(interface{})
}

type Context struct {
	context interface{}
}

func (c *Context) SetContext(context interface{}) {
	c.context = context
}

func (c *Context) Context() interface{} {
	return c.context
}

type Computable struct {
	util.Lifetime
	*Log
	*Context
}

var _ Loggable = (*Computable)(nil)
var _ Contextable = (*Computable)(nil)

func NewComputable(ctx context.Context) *Computable {
	l := util.NewLifetime(ctx)
	return &Computable{
		Lifetime: l,
		Log:      &Log{},
		Context:  &Context{},
	}
}
