package pipe

import (
	"context"

	"github.com/cfoust/cy/pkg/util"
)

type Packet[T any] struct {
	Contents T
	Error    error
}

type Pipe[T any] interface {
	Send(data T) error
	Subscribe(ctx context.Context) *util.Subscriber[Packet[T]]
}
