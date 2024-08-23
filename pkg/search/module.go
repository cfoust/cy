package search

import (
	"context"

	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	tea "github.com/charmbracelet/bubbletea"
)

type Search struct {
	util.Lifetime
	render         *taro.Renderer
	initialRequest *Request

	resultc chan fileResult

	selected          int
	searching         bool
	searchLifetime    *util.Lifetime
	pending, complete []fileResult
}

var _ taro.Model = (*Search)(nil)

func (s *Search) Init() tea.Cmd {
	if s.initialRequest == nil {
		return nil
	}

	_, cmd := s.Execute(*s.initialRequest)
	return cmd
}

func (s *Search) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	switch msg := msg.(type) {
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

		s.complete = make([]fileResult, len(s.pending))
		copy(s.complete, s.pending)
		s.pending = nil
		s.cancelSearch()
		return s, nil
	case ActionEvent:
		switch msg.Type {
		case ActionCancel:
			s.cancelSearch()
			return s, nil
		}
	}

	return s, nil
}

type Option func(*Search)

func WithRequest(req Request) Option {
	return func(s *Search) {
		s.initialRequest = &req
	}
}

func newSearch(ctx context.Context) *Search {
	return &Search{
		Lifetime: util.NewLifetime(ctx),
		render:   taro.NewRenderer(),
	}
}

func New(
	ctx context.Context,
	options ...Option,
) *taro.Program {
	s := newSearch(ctx)

	for _, option := range options {
		option(s)
	}

	return taro.New(ctx, s)
}
