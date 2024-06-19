package taro

import (
	"context"

	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/util"
)

type ScreenUpdate struct {
	w      *ScreenWatcher
	Screen mux.Screen
	Msg    Msg
}

func (s ScreenUpdate) Wait() Cmd {
	if s.w == nil {
		return nil
	}

	return s.w.Wait()
}

type ScreenWatcher struct {
	util.Lifetime
	wait  *util.Lifetime
	queue chan ScreenUpdate
}

func (s *ScreenWatcher) Wait() Cmd {
	return func() Msg {
		select {
		case <-s.Ctx().Done():
			return nil
		case msg := <-s.queue:
			return msg
		}
	}
}

func NewWatcher(ctx context.Context, screen mux.Screen) *ScreenWatcher {
	w := &ScreenWatcher{
		Lifetime: util.NewLifetime(ctx),
		queue:    make(chan ScreenUpdate, 1000),
	}

	u := screen.Subscribe(ctx)

	go func() {
		defer u.Done()
		for {
			select {
			case msg := <-u.Recv():
				select {
				case w.queue <- ScreenUpdate{
					w:      w,
					Screen: screen,
					Msg:    msg,
				}:
				case <-w.Ctx().Done():
					return
				}
			case <-w.Ctx().Done():
				return
			}
		}
	}()

	return w
}

func waitScreen(ctx context.Context, screen mux.Screen, done chan<- ScreenUpdate) {
	u := screen.Subscribe(ctx)
	defer u.Done()

	for {
		select {
		case msg := <-u.Recv():
			select {
			case done <- ScreenUpdate{
				Screen: screen,
				Msg:    msg,
			}:
			case <-ctx.Done():
				return
			}
		case <-ctx.Done():
			return
		}
	}
}

func WaitScreens(ctx context.Context, screens ...mux.Screen) Cmd {
	ctx, cancel := context.WithCancel(ctx)

	done := make(chan ScreenUpdate)
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
			return msg
		case <-ctx.Done():
			return nil
		}
	}
}
