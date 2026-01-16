package main

import (
	"context"
	_ "embed"

	"github.com/cfoust/cy/pkg/anim"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/layout"
	"github.com/cfoust/cy/pkg/mux/screen/placeholder"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	tea "github.com/charmbracelet/bubbletea"
)

func createInitial(size geom.Size) image.Image {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	outerLayers := layout.AddMargins(ctx, placeholder.New(ctx))
	_ = outerLayers.Resize(size)
	return outerLayers.State().Image
}

type AnimationStory struct {
	util.Lifetime
	render   *taro.Renderer
	animator *taro.Program
	creator  anim.Creator
}

var _ taro.Model = (*AnimationStory)(nil)

func (s *AnimationStory) Init() tea.Cmd {
	return taro.WaitScreens(s.animator.Ctx(), s.animator)
}

func (s *AnimationStory) View(state *tty.State) {
	tty.Copy(geom.Size{}, state, s.animator.State())
}

func (s *AnimationStory) initialize(size geom.Size) {
	if s.animator != nil {
		s.animator.Cancel()
	}

	s.animator = anim.NewAnimator(
		s.Ctx(),
		s.creator(),
		createInitial(size),
	)
	_ = s.animator.Resize(size)
}

func (s *AnimationStory) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		s.initialize(geom.Size{
			R: msg.Height,
			C: msg.Width,
		})
		return s, taro.WaitScreens(s.animator.Ctx(), s.animator)
	case taro.ScreenUpdate:
		return s, taro.WaitScreens(s.animator.Ctx(), s.animator)
	}

	return s, nil
}

func NewAnimationStory(
	ctx context.Context,
	creator anim.Creator,
) *taro.Program {
	story := &AnimationStory{
		Lifetime: util.NewLifetime(ctx),
		render:   taro.NewRenderer(),
		creator:  creator,
	}

	story.initialize(geom.DEFAULT_SIZE)

	return taro.New(ctx, story)
}
