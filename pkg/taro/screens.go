package taro

import (
	"context"

	"github.com/cfoust/cy/pkg/mux"
)

type ScreenUpdate struct{}

func WaitScreens(ctx context.Context, screens ...mux.Screen) Cmd {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	done := make(chan struct{})
	for _, screen := range screens {
		s := screen
		go func() {
			u := s.Subscribe(ctx)

			select {
			case <-u.Recv():
				select {
				case done <- struct{}{}:
				case <-ctx.Done():
					return
				}
			case <-ctx.Done():
				return
			}
		}()
	}

	return func() Msg {
		select {
		case <-done:
			return ScreenUpdate{}
		case <-ctx.Done():
			return nil
		}
	}
}
