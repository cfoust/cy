package search

import (
	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/replay"
	"github.com/cfoust/cy/pkg/replay/loader"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
)

func (s *Search) emit(event tea.Msg) tea.Cmd {
	return func() taro.Msg {
		return taro.PublishMsg{
			Msg: event,
		}
	}
}

func (s *Search) resize(size geom.Size) {
	s.size = size
	s.inner = geom.Rect{
		Position: geom.Vec2{
			R: 1,
			C: 0,
		},
		Size: geom.Size{
			R: geom.Max(size.R-1, 0),
			C: size.C,
		},
	}

	// -2 for ~>
	s.input.Width = geom.Max(size.C-2, 0)

	if s.loader == nil {
		return
	}

	s.loader.Resize(s.inner.Size)
}

func (s *Search) setSelected(index int) taro.Cmd {
	newIndex := geom.Max(
		0,
		geom.Clamp(index, 0, len(s.complete)-1),
	)

	if newIndex >= len(s.complete) || newIndex < 0 {
		return nil
	}

	if s.loader != nil && s.selected == newIndex {
		return nil
	}

	s.selected = newIndex

	if s.loader != nil {
		s.loaderLifetime.Cancel()
	}

	result := s.complete[s.selected]

	s.loaderLifetime = util.NewLifetime(s.Ctx())
	r := loader.New(
		s.loaderLifetime.Ctx(),
		s.params,
		s.timeBinds,
		s.copyBinds,
		result.File,
		replay.WithNoQuit,
		replay.WithResults(result.Results),
		replay.WithParams(s.params),
	)
	r.Resize(s.inner.Size)
	s.loader = r
	return taro.NewWatcher(s.loaderLifetime.Ctx(), r).Wait()
}

func (s *Search) handleInput(msg tea.Msg) (taro.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case ActionEvent:
		switch msg.Type {
		case ActionCancel:
			s.inputing = false
			return s, nil
		}
	case taro.KeyMsg:
		switch msg.Type {
		case taro.KeyEsc, taro.KeyCtrlC:
			s.inputing = false
			return s, nil
		case taro.KeyEnter:
			s.inputing = false

			if s.initialRequest == nil {
				return s, nil
			}

			request := *s.initialRequest
			request.Query = s.input.Value()
			return s.Execute(request)
		}
	}

	var cmd tea.Cmd
	inputMsg := msg
	if key, ok := msg.(taro.KeyMsg); ok {
		inputMsg = key.ToTea()
	}
	s.input, cmd = s.input.Update(inputMsg)
	return s, cmd
}

func (s *Search) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case replay.CopyEvent:
		replay := s.loader
		if replay == nil {
			return s, nil
		}

		replay.Send(msg)
		return s, nil
	case taro.ScreenUpdate:
		switch innerMsg := msg.Msg.(type) {
		case bind.BindEvent, replay.CopyEvent:
			return s, taro.Sequence(
				s.emit(innerMsg),
				msg.Wait(),
			)
		}

		return s, msg.Wait()
	case Request:
		return s.Execute(msg)
	case resultEvent:
		return s.handleResult(msg)
	case tea.WindowSizeMsg:
		s.resize(geom.Size{
			R: msg.Height,
			C: msg.Width,
		})
		return s, nil
	case taro.MouseMsg:
		replay := s.loader
		if replay == nil {
			return s, nil
		}

		inner := s.inner

		return s, func() tea.Msg {
			replay.Send(taro.TranslateMouseMessage(
				msg,
				-inner.Position.C,
				-inner.Position.R,
			))
			return nil
		}
	case replay.ActionEvent, replay.PlaybackRateEvent:
		if s.loader == nil {
			return s, nil
		}
		s.loader.Send(msg)
		return s, nil
	case bind.BindEvent:
		return s, s.emit(msg)
	}

	if s.inputing {
		return s.handleInput(msg)
	}

	// The messages below only make sense when we're not inputing
	switch msg := msg.(type) {
	case taro.KeyMsg:
		replay := s.loader
		return s, func() tea.Msg {
			if consumed := s.searchBinds.InputMessage(msg); consumed {
				return nil
			}

			if replay == nil {
				return nil
			}

			replay.Send(msg)
			return nil
		}
	case ActionEvent:
		switch msg.Type {
		case ActionInput:
			s.inputing = true
			s.input.Reset()
			return s, textinput.Blink
		case ActionCancel:
			if s.searching {
				s.cancelSearch()
				return s, nil
			}

			// If these are the same key, we want to send cancel
			// to Replay anyway
			if s.loader != nil {
				s.loader.Send(replay.ActionEvent{
					Type: replay.ActionQuit,
				})
			}

			return s, nil
		case ActionNext, ActionPrev, ActionFirst, ActionLast:
			if !s.haveResults() {
				return s, nil
			}

			switch msg.Type {
			case ActionNext:
				next := s.selected + 1

				if next >= len(s.complete) {
					next = 0
				}

				return s, s.setSelected(next)
			case ActionPrev:
				next := s.selected - 1

				if next < 0 {
					next = len(s.complete) - 1
				}

				return s, s.setSelected(next)
			case ActionFirst:
				return s, s.setSelected(0)
			case ActionLast:
				return s, s.setSelected(len(s.complete) - 1)
			}
			return s, nil
		}
	}

	return s, nil
}
