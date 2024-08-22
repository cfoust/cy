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

	resultc        chan fileResult
	searching      bool
	searchLifetime *util.Lifetime
	results        []fileResult
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
