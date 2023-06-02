package ws

import (
	"context"

	"github.com/cfoust/cy/pkg/io/pipe"
)

type mappedClient[S any, T any] struct {
	client Client[S]
	io     pipe.Pipe[T]
}

func (c *mappedClient[S, T]) Ctx() context.Context {
	return c.client.Ctx()
}

func (c *mappedClient[S, T]) Send(data T) error {
	return c.io.Send(data)
}

func (c *mappedClient[S, T]) Receive() <-chan pipe.Packet[T] {
	return c.io.Receive()
}

func (c *mappedClient[S, T]) Close() error {
	return c.client.Close()
}

// Given a client with messages of type S and encode/decode functions from S
// to T, return a client that uses messages of type T.
// This is a wrapper for pipe.Map.
func MapClient[S any, T any](
	original Client[S],
	encode func(T) (S, error),
	decode func(S) (T, error),
) Client[T] {
	return &mappedClient[S, T]{
		client: original,
		io: pipe.Map[S](
			original,
			encode,
			decode,
		),
	}
}
