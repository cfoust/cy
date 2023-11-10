package stories

import (
	"context"

	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type StoryViewer struct {
	util.Lifetime

	render *taro.Renderer
	replay *taro.Program
}

var _ taro.Model = (*StoryViewer)(nil)

func (s *StoryViewer) Init() tea.Cmd {
	return nil
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
}

func (r *StoryViewer) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case taro.KeyMsg:
		if msg.String() == "q" {
			return r, tea.Quit
		}
	}

	return r, nil
}

func NewViewer(
	ctx context.Context,
) *taro.Program {
	program := taro.New(ctx, &StoryViewer{
		Lifetime: util.NewLifetime(ctx),
		render:   taro.NewRenderer(),
	})
	return program
}
