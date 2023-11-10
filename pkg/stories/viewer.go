package stories

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type StoryViewer struct {
	util.Lifetime

	config Config
	render *taro.Renderer
	screen mux.Screen
}

var _ taro.Model = (*StoryViewer)(nil)

func (s *StoryViewer) Init() tea.Cmd {
	return taro.WaitScreens(s.Ctx(), s.screen)
}

func (s *StoryViewer) View(state *tty.State) {
	size := state.Image.Size()
	s.render.RenderAt(
		state.Image,
		0, 0,
		lipgloss.Place(
			size.C,
			size.R,
			lipgloss.Center, lipgloss.Center,
			"test",
		),
	)

	contents := s.screen.State()
	storySize := contents.Image.Size()
	storyPos := geom.Vec2{
		R: (size.R / 2) - (storySize.R / 2),
		C: (size.C / 2) - (storySize.C / 2),
	}

	state.Image.Clear(geom.Rect{
		R: storyPos.R,
		C: storyPos.C,
		H: storySize.R,
		W: storySize.C,
	})
	tty.Copy(storyPos, state, contents)
	state.CursorVisible = contents.CursorVisible
}

func (s *StoryViewer) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		if s.config.Size.IsZero() {
			s.screen.Resize(geom.Size{
				R: msg.Height,
				C: msg.Width,
			})
		}
		return s, nil
	case taro.ScreenUpdate:
		return s, taro.WaitScreens(s.Ctx(), s.screen)
	case taro.KeyMsg:
		if msg.String() == "q" {
			return s, tea.Quit
		}
	}

	return s, nil
}

func NewViewer(
	ctx context.Context,
	screen mux.Screen,
	config Config,
) *taro.Program {
	program := taro.New(ctx, &StoryViewer{
		Lifetime: util.NewLifetime(ctx),
		render:   taro.NewRenderer(),
		config:   config,
		screen:   screen,
	})
	return program
}