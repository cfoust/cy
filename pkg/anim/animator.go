package anim

import (
	"context"
	"time"

	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
)

type Animator struct {
	animation Animation
	start     time.Time
	state     image.Image
	fps       int
}

var _ taro.Model = (*Animator)(nil)

func (a *Animator) Init() tea.Cmd {
	return a.waitFrame
}

func (a *Animator) waitFrame() tea.Msg {
	time.Sleep(time.Second / time.Duration(a.fps))
	return refresh{}
}

func (a *Animator) View(state *tty.State) {
	image.Copy(state.Image.Size().Center(a.state.Size()), state.Image, a.state)
	state.CursorVisible = false
}

type refresh struct{}

func (a *Animator) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	switch msg.(type) {
	case refresh:
		now := time.Now()
		if now.After(a.start) {
			a.state = a.animation.Update(now.Sub(a.start))
		}

		return a, a.waitFrame
	}

	return a, nil
}

type Option func(*Animator)

func WithStartTime(start time.Time) Option {
	return func(a *Animator) {
		a.start = start
	}
}

func NewAnimator(
	ctx context.Context,
	animation Animation,
	initial image.Image,
	fps int,
	options ...Option,
) *taro.Program {
	a := &Animator{
		animation: animation,
		start:     time.Now(),
		fps:       fps,
	}

	for _, option := range options {
		option(a)
	}

	animation.Init(initial)

	return taro.New(ctx, a)
}
