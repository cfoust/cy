package ui

import (
	"context"

	"github.com/cfoust/cy/pkg/fuzzy"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	S "github.com/cfoust/cy/pkg/stories"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	tea "github.com/charmbracelet/bubbletea"
)

// A Browser lets the user switch between different stories.
type Browser struct {
	util.Lifetime
	size           geom.Vec2
	render         *taro.Renderer
	fuzzy          *taro.Program
	viewer         *taro.Program
	viewerLifetime util.Lifetime
	watcher        *taro.ScreenWatcher
}

var _ taro.Model = (*Browser)(nil)

func (s *Browser) Init() tea.Cmd {
	return s.watcher.Wait()
}

func (s *Browser) View(state *tty.State) {
	tty.Copy(geom.Vec2{}, state, s.fuzzy.State())

	if s.viewer == nil {
		return
	}

	viewState := s.viewer.State()
	tty.Copy(geom.Vec2{C: 30}, state, viewState)
}

type loadedStory struct {
	screen   *taro.Program
	lifetime util.Lifetime
}

func (s *Browser) loadStory(story S.Story) tea.Cmd {
	size := s.size
	size.C -= 30
	return func() tea.Msg {
		lifetime := util.NewLifetime(s.Ctx())
		screen := story.Init(lifetime.Ctx())
		config := story.Config
		if !config.Size.IsZero() {
			screen.Resize(config.Size)
		}

		viewer := NewViewer(
			lifetime.Ctx(),
			screen,
			config,
		)
		viewer.Resize(size)
		return loadedStory{screen: viewer, lifetime: lifetime}
	}
}

func (s *Browser) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case loadedStory:
		s.viewer = msg.screen
		s.viewerLifetime = msg.lifetime
	case tea.WindowSizeMsg:
		size := geom.Size{
			R: msg.Height,
			C: msg.Width,
		}
		s.size = size
		s.fuzzy.Resize(geom.Size{
			R: msg.Height,
			C: 30,
		})

		if s.viewer != nil {
			s.viewer.Resize(geom.Size{
				R: msg.Height,
				C: msg.Width - 30,
			})
		}
		return s, nil
	case taro.ScreenUpdate:
		cmds := []taro.Cmd{
			s.watcher.Wait(),
		}

		if s.viewer != nil {
			cmds = append(cmds, taro.WaitScreens(
				s.Ctx(),
				s.viewer,
			))
		}

		switch msg := msg.Msg.(type) {
		case fuzzy.SelectedEvent:
			if story, ok := msg.Option.Result.(S.Story); ok {
				if s.viewer != nil {
					s.viewer.Cancel()
					s.viewerLifetime.Cancel()
					s.viewer = nil
				}

				cmds = append(
					cmds,
					s.loadStory(story),
				)
				return s, tea.Batch(cmds...)
			}
		}

		return s, tea.Batch(cmds...)
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
	stories []S.Story,
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

	fuzzy := fuzzy.NewFuzzy(
		ctx,
		options,
		fuzzy.WithSticky,
		fuzzy.WithReverse,
		fuzzy.WithPrompt("choose a story"),
	)
	browser := &Browser{
		Lifetime: util.NewLifetime(ctx),
		render:   taro.NewRenderer(),
		fuzzy:    fuzzy,
		watcher:  taro.NewWatcher(ctx, fuzzy),
	}

	program := taro.New(ctx, browser)
	return program
}
