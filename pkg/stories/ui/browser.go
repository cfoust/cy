package ui

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/input/fuzzy"
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
}

var _ taro.Model = (*Browser)(nil)

func (s *Browser) Init() tea.Cmd {
	watcher := taro.NewWatcher(s.Ctx(), s.fuzzy)
	return watcher.Wait()
}

func (s *Browser) View(state *tty.State) {
	tty.Copy(geom.Vec2{}, state, s.fuzzy.State())

	if s.viewer == nil {
		return
	}

	viewState := s.viewer.State()
	tty.Copy(geom.Vec2{C: 30}, state, viewState)
}

type loadedViewer struct {
	viewer   *taro.Program
	lifetime util.Lifetime
	watcher  *taro.ScreenWatcher
}

func (s *Browser) loadViewer(story S.Story) tea.Cmd {
	size := s.size
	size.C -= 30
	return func() tea.Msg {
		lifetime := util.NewLifetime(s.Ctx())
		viewer := NewViewer(lifetime.Ctx(), story, true)
		watcher := taro.NewWatcher(lifetime.Ctx(), viewer)
		_ = viewer.Resize(size)
		return loadedViewer{
			viewer:   viewer,
			lifetime: lifetime,
			watcher:  watcher,
		}
	}
}

func (s *Browser) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case loadedViewer:
		s.viewer = msg.viewer
		s.viewerLifetime = msg.lifetime
		return s, msg.watcher.Wait()
	case tea.WindowSizeMsg:
		size := geom.Size{
			R: msg.Height,
			C: msg.Width,
		}
		s.size = size
		_ = s.fuzzy.Resize(geom.Size{
			R: msg.Height,
			C: 30,
		})

		if s.viewer != nil {
			_ = s.viewer.Resize(geom.Size{
				R: msg.Height,
				C: msg.Width - 30,
			})
		}
		return s, nil
	case taro.ScreenUpdate:
		cmds := []taro.Cmd{
			msg.Wait(),
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
					s.loadViewer(story),
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

	fuzzy := fuzzy.New(
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
	}

	program := taro.New(ctx, browser)
	return program
}
