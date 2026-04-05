package cy

import (
	"context"
	"fmt"
	"time"

	"github.com/sasha-s/go-deadlock"
)

type signalRegistry struct {
	deadlock.Mutex
	channels map[string][]chan struct{}
}

func newSignalRegistry() *signalRegistry {
	return &signalRegistry{
		channels: make(map[string][]chan struct{}),
	}
}

// Signal wakes all goroutines waiting on the named channel.
func (s *signalRegistry) Signal(name string) {
	s.Lock()
	waiters := s.channels[name]
	delete(s.channels, name)
	s.Unlock()

	for _, ch := range waiters {
		close(ch)
	}
}

// WaitFor blocks until the named channel is signaled or the context
// is canceled. An optional timeout (in seconds) limits how long to
// wait; 0 means no timeout.
func (s *signalRegistry) WaitFor(
	ctx context.Context,
	name string,
	timeoutSecs float64,
) error {
	ch := make(chan struct{})

	s.Lock()
	s.channels[name] = append(s.channels[name], ch)
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

	select {
	case <-ch:
		return nil
	case <-timeoutCh:
		// Remove this waiter from the registry
		s.Lock()
		waiters := s.channels[name]
		for i, w := range waiters {
			if w == ch {
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
		return fmt.Errorf(
			"timed out waiting for signal %q",
			name,
		)
	case <-ctx.Done():
		return ctx.Err()
	}
}
