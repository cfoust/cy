package util

import (
	"context"
	"time"
)

type Session struct {
	context   context.Context
	cancel    context.CancelFunc
	startTime time.Time
}

func NewSession(ctx context.Context) Session {
	ctx, cancel := context.WithCancel(ctx)
	return Session{
		context:   ctx,
		cancel:    cancel,
		startTime: time.Now(),
	}
}

func (s *Session) Started() time.Time {
	return s.startTime
}

func (s *Session) Ctx() context.Context {
	return s.context
}

func (s *Session) IsDone() bool {
	return s.context.Err() != nil
}

func (s *Session) Cancel() {
	s.cancel()
}
