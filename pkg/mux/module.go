package mux

import (
	"io"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/util"
)

type Size = geom.Vec2

type Updater = util.Subscriber[*tty.State]
type UpdatePublisher = util.Publisher[*tty.State]

func NewPublisher() *UpdatePublisher {
	return util.NewPublisher[*tty.State]()
}

type Resizable interface {
	Resize(size Size) error
}

// A Stream accepts input (Write) and resize events and produces output (Read).
type Stream interface {
	io.ReadWriter
	Resizable
}

// A Screen is the "end" of a chain of IO, or the result that the user sees.
type Screen interface {
	io.Writer
	State() *tty.State
	Updates() *Updater
	Resizable
}
