package anim

import (
	"context"
	"time"

	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
)

// We assume frames need about this long to render on the client
const defaultRenderBuffer = 5 * time.Millisecond

type Animator struct {
	animation     Animation
	start         time.Time
	state         image.Image
	fps           int
	frameDuration time.Duration
	lastFrame     time.Time
}

var _ taro.Model = (*Animator)(nil)

func (a *Animator) Init() tea.Cmd {
	a.state = a.animation.Update(0)
	a.lastFrame = time.Now()
	return a.scheduleNextFrame
}

func (a *Animator) scheduleNextFrame() tea.Msg {
	now := time.Now()
	elapsed := now.Sub(a.lastFrame)
	remaining := a.frameDuration - elapsed - defaultRenderBuffer

	// If we're behind schedule, drop frames to catch up
	if remaining > 0 {
		time.Sleep(remaining)
	}

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
		// Track when this frame started for scheduling the next one
		a.lastFrame = time.Now()

		now := a.lastFrame
		if now.After(a.start) {
			a.state = a.animation.Update(now.Sub(a.start))
		}

		return a, a.scheduleNextFrame
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
	frameDuration := time.Second / time.Duration(fps)

	a := &Animator{
		animation:     animation,
		start:         time.Now(),
		fps:           fps,
		frameDuration: frameDuration,
	}

	for _, option := range options {
		option(a)
	}

	animation.Init(initial)

	return taro.New(ctx, a)
}
