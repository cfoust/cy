package search

import (
	"context"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	tea "github.com/charmbracelet/bubbletea"
)

type Search struct {
	util.Lifetime
	render         *taro.Renderer
	initialRequest *Request
	size           geom.Size
	inner          geom.Rect

	resultc chan fileResult

	selected       int
	searching      bool
	searchLifetime *util.Lifetime

	pendingQuery, query string
	pending, complete   []fileResult

	replay         mux.Screen
	replayLifetime util.Lifetime

	searchBinds          *bind.Engine[bind.Action]
	timeBinds, copyBinds *bind.BindScope
}

var _ taro.Model = (*Search)(nil)

func (s *Search) Init() tea.Cmd {
	if s.initialRequest == nil {
		return nil
	}

	_, cmd := s.Execute(*s.initialRequest)
	return cmd
}

func (s *Search) haveResults() bool {
	return len(s.complete) > 0
}

type Option func(*Search)

func WithRequest(req Request) Option {
	return func(s *Search) {
		s.initialRequest = &req
	}
}

func newSearch(
	ctx context.Context,
	searchBinds *bind.Engine[bind.Action],
	timeBinds, copyBinds *bind.BindScope,
) *Search {
	return &Search{
		Lifetime:    util.NewLifetime(ctx),
		render:      taro.NewRenderer(),
		searchBinds: searchBinds,
		timeBinds:   timeBinds,
		copyBinds:   copyBinds,
	}
}

func New(
	ctx context.Context,
	searchBinds, timeBinds, copyBinds *bind.BindScope,
	options ...Option,
) *taro.Program {
	engine := bind.Run(ctx, searchBinds)
	s := newSearch(ctx, engine, timeBinds, copyBinds)

	for _, option := range options {
		option(s)
	}

	program := taro.New(ctx, s)
	go func() {
		for {
			select {
			case <-ctx.Done():
				return
			case event := <-engine.Recv():
				if bindEvent, ok := event.(bind.BindEvent); ok {
					program.Send(bindEvent)
				}
			}
		}
	}()

	return program
}
