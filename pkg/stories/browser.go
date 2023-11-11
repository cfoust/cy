package stories

import (
	"context"

	"github.com/cfoust/cy/pkg/fuzzy"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/rs/zerolog/log"
)

// A Browser lets the user switch between different stories.
type Browser struct {
	util.Lifetime
	render  *taro.Renderer
	fuzzy   *taro.Program
	viewer  *taro.Program
	watcher *taro.ScreenWatcher
}

var _ taro.Model = (*Browser)(nil)

func (s *Browser) Init() tea.Cmd {
	return s.watcher.Wait()
}

func (s *Browser) View(state *tty.State) {
	tty.Copy(geom.Vec2{}, state, s.fuzzy.State())
}

func (s *Browser) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		s.fuzzy.Resize(geom.Size{
			R: msg.Height,
			C: msg.Width,
		})
		return s, nil
	case taro.ScreenUpdate:
		switch msg := msg.Msg.(type) {
		case fuzzy.SelectedEvent:
			log.Info().Msgf("selected %#v", msg)
		}
		return s, s.watcher.Wait()
	case taro.KeyMsg:
		switch msg.String() {
		case "q":
			return s, tea.Quit
		}
		s.fuzzy.Send(msg)
	}

	return s, nil
}

func NewBrowser(
	ctx context.Context,
	stories []Story,
) *taro.Program {
	options := make([]fuzzy.Option, 0)
	for _, story := range stories {
		options = append(
			options,
			fuzzy.NewOption(
				story.Name,
				story,
			),
		)
	}

	fuzzy := fuzzy.NewFuzzy(ctx, options, fuzzy.WithReverse)
	browser := &Browser{
		Lifetime: util.NewLifetime(ctx),
		render:   taro.NewRenderer(),
		fuzzy:    fuzzy,
		watcher:  taro.NewWatcher(ctx, fuzzy),
	}

	program := taro.New(ctx, browser)
	return program
}
