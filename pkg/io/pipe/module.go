package pipe

type Packet[T any] struct {
	Contents T
	Error    error
}

type Pipe[T any] interface {
	Send(data T) error
	Receive() <-chan Packet[T]
}
