package anim

import (
	"context"
	"math/rand"
	"time"

	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/screen"
)

type Animation interface {
	Init(image.Image)
	Update(time.Duration) image.Image
}

type Animator struct {
	*screen.Trigger
	animation Animation
	last      image.Image
	start     time.Time
}

var _ mux.Screen = (*Animator)(nil)

func (a *Animator) Write(data []byte) (n int, err error) {
	return 0, nil
}

func (a *Animator) Render(size mux.Size) *tty.State {
	image := a.animation.Update(time.Now().Sub(a.start))
	state := tty.New(image.Size())
	state.Image = image
	state.CursorVisible = false
	return state
}

func (a *Animator) poll(ctx context.Context, fps int) {
	t := time.NewTicker(time.Second / time.Duration(fps))
	for {
		select {
		case <-ctx.Done():
			return
		case <-t.C:
			a.Rerender()
		}
	}
}

func NewAnimator(
	ctx context.Context,
	animation Animation,
	initial image.Image,
	fps int,
) *Animator {
	a := &Animator{
		animation: animation,
		start:     time.Now(),
	}
	a.Trigger = screen.NewTrigger(a)

	animation.Init(initial)
	a.last = animation.Update(0)
	go a.poll(ctx, fps)

	return a
}

func Random() Animation {
	anims := []Animation{
		&Midjo{},
		&Cyform{},
		&Conway{},
	}

	return anims[rand.Int()%len(anims)]
}
