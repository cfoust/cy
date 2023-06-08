package util

import (
	"context"
	"time"
)

type Lifetime struct {
	context   context.Context
	cancel    context.CancelFunc
	startTime time.Time
}

func NewLifetime(ctx context.Context) Lifetime {
	ctx, cancel := context.WithCancel(ctx)
	return Lifetime{
		context:   ctx,
		cancel:    cancel,
		startTime: time.Now(),
	}
}

func (s *Lifetime) Started() time.Time {
	return s.startTime
}

func (s *Lifetime) Ctx() context.Context {
	return s.context
}

func (s *Lifetime) IsDone() bool {
	return s.context.Err() != nil
}

func (s *Lifetime) Cancel() {
	s.cancel()
}
