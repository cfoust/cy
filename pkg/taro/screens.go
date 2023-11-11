package taro

import (
	"context"

	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/util"
)

type ScreenUpdate struct {
	Msg Msg
}

type ScreenWatcher struct {
	util.Lifetime
	screen mux.Screen
	queue  chan Msg
}

func (s *ScreenWatcher) Wait() Cmd {
	return func() Msg {
		select {
		case <-s.Ctx().Done():
			return nil
		case msg := <-s.queue:
			return ScreenUpdate{Msg: msg}
		}
	}
}

func NewWatcher(ctx context.Context, screen mux.Screen) *ScreenWatcher {
	s := ScreenWatcher{
		Lifetime: util.NewLifetime(ctx),
		queue:    make(chan Msg, 1000),
	}

	go func() {
		ctx := s.Ctx()
		u := screen.Subscribe(ctx)

		for {
			select {
			case msg := <-u.Recv():
				select {
				case s.queue <- msg:
				case <-ctx.Done():
					return
				}
			case <-ctx.Done():
				return
			}
		}
	}()

	return &s
}

func WaitScreens(ctx context.Context, screens ...mux.Screen) Cmd {
	ctx, cancel := context.WithCancel(ctx)

	done := make(chan Msg)
	for _, screen := range screens {
		s := screen
		go func() {
			u := s.Subscribe(ctx)

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
		}()
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
