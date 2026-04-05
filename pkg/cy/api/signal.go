package api

import (
	"context"

	"github.com/cfoust/cy/pkg/janet"
)

type SignalSender interface {
	Signal(name string)
	WaitFor(ctx context.Context, name string, timeoutSecs float64) error
}

type SignalModule struct {
	Signals SignalSender
}

func (s *SignalModule) Send(name janet.Keyword) {
	s.Signals.Signal(string(name))
}

type SignalWaitParams struct {
	Timeout float64
}

func (s *SignalModule) Wait(
	ctx context.Context,
	name janet.Keyword,
	params *janet.Named[SignalWaitParams],
) error {
	values := params.Values()
	return s.Signals.WaitFor(ctx, string(name), values.Timeout)
}
