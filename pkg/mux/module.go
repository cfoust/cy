package mux

import (
	"context"
	"io"

	"github.com/cfoust/cy/pkg/events"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/util"
)

type Msg = events.Msg
type Size = geom.Vec2

type Updater = util.Subscriber[events.Msg]
type UpdatePublisher struct {
	*util.Publisher[events.Msg]
}

func (u *UpdatePublisher) Notify() {
	u.Publish(nil)
}

func NewPublisher() *UpdatePublisher {
	return &UpdatePublisher{
		Publisher: util.NewPublisher[events.Msg](),
	}
}

type Resizable interface {
	// Resize sets the screen dimensions of the entity to size.
	Resize(size Size) error
}

type Killable interface {
	// Kill stops all activity in this Screen.
	Kill()
}

// A Stream accepts input (Write) and resize events and produces output (Read).
type Stream interface {
	io.ReadWriter
	Resizable
	Killable
}

// A Screen is a resizable box that can publish updates.
type Screen interface {
	Resizable
	Killable
	// State gets the visible state of the Screen.
	State() *tty.State
	// Subscribe subscribes to any changes to this Screen (typically
	// changes to its visible state.)
	Subscribe(context.Context) *Updater
	// Send sends an arbitrary message to the Screen.
	Send(message events.Msg)
}
