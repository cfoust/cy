package channel

import (
	"context"
)

type Packet[T any] struct {
	Contents T
	Error    error
}

type Pipe[T any] interface {
	Ctx() context.Context
	Send(data T) error
	Receive() <-chan Packet[T]
}

