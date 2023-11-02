package splash

import (
	"context"

	"github.com/cfoust/cy/pkg/anim"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	tea "github.com/charmbracelet/bubbletea"
)

type Splash struct {
	util.Lifetime
	anim   *anim.Animator
	render *taro.Renderer
}

var _ taro.Model = (*Splash)(nil)

func (s *Splash) Init() taro.Cmd {
	return taro.WaitScreens(s.Ctx(), s.anim)
}

func (s *Splash) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	switch msg.(type) {
	case taro.ScreenUpdate:
		return s, taro.WaitScreens(s.Ctx(), s.anim)
	}

	return s, nil
}

func (s *Splash) View(state *tty.State) {
	size := state.Image.Size()
	state.CursorVisible = false

	bg := s.anim.State().Image
	bgSize := bg.Size()
	image.Copy(
		geom.Vec2{
			R: (size.R / 2) - (bgSize.R / 2),
			C: (size.C / 2) - (bgSize.C / 2),
		},
		state.Image,
		bg,
	)
}

func New(ctx context.Context, size geom.Size) *taro.Program {
	render := taro.NewRenderer()
	return taro.New(ctx, &Splash{
		Lifetime: util.NewLifetime(ctx),
		render:   render,
		anim: anim.NewAnimator(
			ctx,
			&anim.Midjo{},
			generateBackground(render, size.Scalar(2)),
			23,
		),
	})
}
