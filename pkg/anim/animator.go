package anim

import (
	"context"
	"time"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/util"

	"github.com/sasha-s/go-deadlock"
)

type Animator struct {
	util.Lifetime
	deadlock.RWMutex
	*mux.UpdatePublisher
	animation Animation
	last      image.Image
	start     time.Time
	size      geom.Size
}

var _ mux.Screen = (*Animator)(nil)

func (a *Animator) Send(msg mux.Msg) {
}

func (a *Animator) State() *tty.State {
	a.RLock()
	size := a.size
	a.RUnlock()
	return a.Render(size)
}

func (a *Animator) Resize(size geom.Size) error {
	a.Lock()
	a.size = size
	a.Unlock()
	a.Notify()
	return nil
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
			a.Notify()
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
		Lifetime:        util.NewLifetime(ctx),
		UpdatePublisher: mux.NewPublisher(),
		animation:       animation,
		start:           time.Now(),
	}

	animation.Init(initial)
	a.last = animation.Update(0)
	go a.poll(a.Ctx(), fps)

	return a
}
