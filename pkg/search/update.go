package search

import (
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/replay"
	"github.com/cfoust/cy/pkg/replay/loader"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	tea "github.com/charmbracelet/bubbletea"
)

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

	if s.replay == nil {
		return
	}

	s.replay.Resize(s.inner.Size)
}

func (s *Search) setSelected(index int) taro.Cmd {
	s.selected = geom.Max(
		0,
		geom.Clamp(index, 0, len(s.complete)-1),
	)

	if s.selected >= len(s.complete) || s.selected < 0 {
		return nil
	}

	if s.replay != nil {
		s.replayLifetime.Cancel()
	}

	s.replayLifetime = util.NewLifetime(s.Ctx())
	r := loader.New(
		s.replayLifetime.Ctx(),
		s.complete[s.selected].File,
		s.timeBinds,
		s.copyBinds,
		replay.WithNoQuit,
	)
	r.Resize(s.inner.Size)
	s.replay = r
	return taro.NewWatcher(s.replayLifetime.Ctx(), r).Wait()
}

func (s *Search) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case taro.ScreenUpdate:
		return s, msg.Wait()
	case Request:
		return s.Execute(msg)
	case resultEvent:
		s.pending[msg.fileResult.ID] = msg.fileResult

		allDone := true
		for _, result := range s.pending {
			if result.Done {
				continue
			}
			allDone = false
		}

		if !allDone {
			return s, s.waitResult()
		}

		var complete []fileResult
		for _, result := range s.pending {
			if len(result.Results) == 0 {
				continue
			}
			complete = append(complete, result)
		}

		s.complete = complete
		s.pending = nil
		s.cancelSearch()
		return s, s.setSelected(0)
	case ActionEvent:
		switch msg.Type {
		case ActionCancel:
			s.cancelSearch()
			return s, nil
		}
	case tea.WindowSizeMsg:
		s.resize(geom.Size{
			R: msg.Height,
			C: msg.Width,
		})
		return s, nil
	}

	return s, nil
}
