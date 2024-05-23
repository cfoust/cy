package taro

import (
	"context"

	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/util"

	"github.com/sasha-s/go-deadlock"
)

type ScreenUpdate struct {
	Msg Msg
}

func waitScreen(ctx context.Context, screen mux.Screen, done chan<- Msg) {
	u := screen.Subscribe(ctx)
	defer u.Done()

	for {
		select {
		case msg := <-u.Recv():
			select {
			case done <- msg:
			case <-ctx.Done():
				return
			}
		case <-ctx.Done():
			return
		}
	}
}

type ScreenWatcher struct {
	util.Lifetime
	mu    deadlock.RWMutex
	wait  *util.Lifetime
	queue chan Msg
}

func (s *ScreenWatcher) Wait(screens ...mux.Screen) Cmd {
	wait := util.NewLifetime(s.Ctx())

	s.mu.Lock()
	if s.wait != nil {
		s.wait.Cancel()
	}
	s.wait = &wait
	s.mu.Unlock()

	for _, screen := range screens {
		// For convenience
		if screen == nil {
			continue
		}

		go waitScreen(wait.Ctx(), screen, s.queue)
	}

	return func() Msg {
		select {
		case <-wait.Ctx().Done():
			return nil
		case msg := <-s.queue:
			wait.Cancel()

			s.mu.Lock()
			s.wait = nil
			s.mu.Unlock()

			return ScreenUpdate{Msg: msg}
		}
	}
}

func NewWatcher(ctx context.Context) *ScreenWatcher {
	return &ScreenWatcher{
		Lifetime: util.NewLifetime(ctx),
		queue:    make(chan Msg, 1000),
	}
}

func WaitScreens(ctx context.Context, screens ...mux.Screen) Cmd {
	ctx, cancel := context.WithCancel(ctx)

	done := make(chan Msg)
	for _, screen := range screens {
		// For convenience
		if screen == nil {
			continue
		}

		go waitScreen(ctx, screen, done)
	}

	return func() Msg {
		defer cancel()
		select {
		case msg := <-done:
			return ScreenUpdate{Msg: msg}
		case <-ctx.Done():
			return nil
		}
	}
}
