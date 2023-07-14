package app

import (
	"io"
	"time"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/util"
)

type Size = geom.Size

type Notifier = util.Subscriber[time.Time]

type Resizable interface {
	Resize(size Size) error
}

// An IO accepts input (Write) and resize events and produces output (Read).
type IO interface {
	io.ReadWriter
	Resizable
}

// A Screen is the "end" of a chain of IO, or the result that the user sees.
type Screen interface {
	io.Writer
	View() emu.View
	Updates() *Notifier
	Resizable
}
