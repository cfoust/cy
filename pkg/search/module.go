package search

import (
	"context"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/params"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
)

type Search struct {
	util.Lifetime
	params         *params.Parameters
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

	loader         mux.Screen
	loaderLifetime util.Lifetime

	searchBinds          *bind.Engine[bind.Action]
	timeBinds, copyBinds *bind.BindScope

	inputing bool
	input    textinput.Model
}

var _ taro.Model = (*Search)(nil)

func (s *Search) Init() tea.Cmd {
	cmds := []tea.Cmd{
		textinput.Blink,
	}

	if s.initialRequest != nil {
		_, cmd := s.Execute(*s.initialRequest)
		cmds = append(cmds, cmd)
	}

	return tea.Batch(cmds...)
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

// WithParams provides parameters this Replay will use for rendering.
func WithParams(params *params.Parameters) Option {
	return func(s *Search) {
		s.params = params
	}
}

func newSearch(
	ctx context.Context,
	searchBinds *bind.Engine[bind.Action],
	timeBinds, copyBinds *bind.BindScope,
) *Search {
	input := textinput.New()
	input.Focus()
	input.CharLimit = 0
	input.Width = 20
	input.Prompt = ""

	return &Search{
		Lifetime:    util.NewLifetime(ctx),
		params:      params.New(),
		render:      taro.NewRenderer(),
		searchBinds: searchBinds,
		timeBinds:   timeBinds,
		copyBinds:   copyBinds,
		input:       input,
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

	program := taro.New(ctx, s, taro.WithKittyKeys)
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
