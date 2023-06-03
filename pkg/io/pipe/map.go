package pipe

type mappedPipe[S any, T any] struct {
	original Pipe[S]
	encode   func(T) (S, error)
	decode   func(S) (T, error)
}

func (t *mappedPipe[S, T]) Send(data T) error {
	encoded, err := t.encode(data)
	if err != nil {
		return err
	}

	return t.original.Send(encoded)
}

func (t *mappedPipe[S, T]) Receive() <-chan Packet[T] {
	before := t.original.Receive()
	after := make(chan Packet[T])

	go func() {
		for {
			select {
			case msg, more := <-before:
				if !more {
					close(after)
					return
				}

				if msg.Error != nil {
					after <- Packet[T]{
						Error: msg.Error,
					}
					close(after)
					return
				}

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

func Map[S any, T any](
	original Pipe[S],
	encode func(T) (S, error),
	decode func(S) (T, error),
) Pipe[T] {
	return &mappedPipe[S, T]{
		original: original,
		encode:   encode,
		decode:   decode,
	}
}
