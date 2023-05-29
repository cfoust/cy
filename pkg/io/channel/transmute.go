package channel

import (
	"context"
)

type transmutedPipe[S any, T any] struct {
	original Pipe[S]
	encode   func(T) (S, error)
	decode   func(S) (T, error)
}

func (t *transmutedPipe[S, T]) Ctx() context.Context {
	return t.original.Ctx()
}

func (t *transmutedPipe[S, T]) Send(data T) error {
	encoded, err := t.encode(data)
	if err != nil {
		return err
	}

	return t.original.Send(encoded)
}

func (t *transmutedPipe[S, T]) Receive() <-chan Packet[T] {
	before := t.original.Receive()
	after := make(chan Packet[T])

	go func() {
		for {
			select {
			case <-t.original.Ctx().Done():
				return
			case msg := <-before:
				decoded, err := t.decode(msg.Contents)
				after <- Packet[T]{
					Contents: decoded,
					Error:    err,
				}
			}
		}
	}()

	return after
}

func Transmute[S any, T any](
	original Pipe[S],
	encode func(T) (S, error),
	decode func(S) (T, error),
) Pipe[T] {
	return &transmutedPipe[S, T]{
		original: original,
		encode:   encode,
		decode:   decode,
	}
}
