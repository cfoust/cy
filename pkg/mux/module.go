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
	Resize(size Size) error
}

// A Stream accepts input (Write) and resize events and produces output (Read).
type Stream interface {
	io.ReadWriter
	Resizable
}

// A Screen is a resizable box that can publish updates.
type Screen interface {
	State() *tty.State
	Subscribe(context.Context) *Updater
	Resizable
	Send(message events.Msg)
}
