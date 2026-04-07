package util

import (
	"context"
	"fmt"
	"time"

	"github.com/sasha-s/go-deadlock"
)

type signalWaiter struct {
	ch chan interface{}
}

type SignalRegistry struct {
	deadlock.Mutex
	channels map[string][]*signalWaiter
}

func NewSignalRegistry() *SignalRegistry {
	return &SignalRegistry{
		channels: make(map[string][]*signalWaiter),
	}
}

// Signal wakes all goroutines waiting on the named channel,
// delivering the given value to each.
func (s *SignalRegistry) Signal(name string, value interface{}) {
	s.Lock()
	waiters := s.channels[name]
	delete(s.channels, name)
	s.Unlock()

	for _, w := range waiters {
		w.ch <- value
		close(w.ch)
	}
}

// WaitFor blocks until the named channel is signaled or the context
// is canceled. An optional timeout (in seconds) limits how long to
// wait; 0 means no timeout. Returns the value passed to Signal.
func (s *SignalRegistry) WaitFor(
	ctx context.Context,
	name string,
	timeoutSecs float64,
) (interface{}, error) {
	w := &signalWaiter{ch: make(chan interface{}, 1)}

	s.Lock()
	s.channels[name] = append(s.channels[name], w)
	s.Unlock()

	var timer *time.Timer
	var timeoutCh <-chan time.Time
	if timeoutSecs > 0 {
		timer = time.NewTimer(
			time.Duration(
				timeoutSecs * float64(time.Second),
			),
		)
		defer timer.Stop()
		timeoutCh = timer.C
	}

	cleanup := func() {
		s.Lock()
		waiters := s.channels[name]
		for i, existing := range waiters {
			if existing == w {
				s.channels[name] = append(
					waiters[:i],
					waiters[i+1:]...,
				)
				break
			}
		}
		if len(s.channels[name]) == 0 {
			delete(s.channels, name)
		}
		s.Unlock()
	}

	select {
	case value := <-w.ch:
		return value, nil
	case <-timeoutCh:
		cleanup()
		return nil, fmt.Errorf(
			"timed out waiting for signal %q",
			name,
		)
	case <-ctx.Done():
		cleanup()
		return nil, ctx.Err()
	}
}
