package splash

import (
	"context"
	"fmt"

	"github.com/cfoust/cy/pkg/anim"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"
	"github.com/cfoust/cy/pkg/version"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
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
	case taro.KeyMsg:
		return s, tea.Quit
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

	boxContents := lipgloss.JoinVertical(
		lipgloss.Center,
		lipgloss.NewStyle().
			Width(50).
			Align(lipgloss.Center).
			Render("CY - Cy IMProved"),
		"",
		fmt.Sprintf("version %s", version.Version),
		"",
		"by Caleb Foust",
		"cy is open source and freely distributable",
		"",
		s.render.NewStyle().
			Foreground(lipgloss.Color("2")).
			Render("press any key to continue"),
	)

	boxStyle := s.render.NewStyle().
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("7")).
		BorderTop(true).
		BorderLeft(true).
		BorderRight(true).
		BorderBottom(true)

	boxText := boxStyle.Render(boxContents)
	boxSize := geom.Vec2{
		R: lipgloss.Height(boxText),
		C: lipgloss.Width(boxText),
	}

	box := image.New(boxSize)
	s.render.RenderAt(
		box,
		0,
		0,
		boxText,
	)

	image.Copy(
		geom.Vec2{
			R: (size.R / 2) - (boxSize.R / 2),
			C: (size.C / 2) - (boxSize.C / 2),
		},
		state.Image,
		box,
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
