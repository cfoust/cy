package thumbs

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/params"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
)

// Thumbs represents the thumbs picker interface
type Thumbs struct {
	util.Lifetime
	params *params.Parameters

	// The initial state of the screen before thumbs was started
	initial image.Image
	result  chan<- interface{}
	size    geom.Vec2

	render    *taro.Renderer
	textInput textinput.Model

	location geom.Vec2

	hints map[string]Match

	// The alphabet to use for generating hints
	alphabet []rune
}

var _ taro.Model = (*Thumbs)(nil)

func (t *Thumbs) quit() (taro.Model, tea.Cmd) {
	return t, tea.Batch(
		func() tea.Msg {
			t.Cancel()
			return nil
		},
		tea.Quit,
	)
}

func (t *Thumbs) Init() taro.Cmd {
	cmds := []taro.Cmd{
		textinput.Blink,
	}

	return tea.Batch(cmds...)
}

type Setting func(context.Context, *Thumbs)

func WithResult(result chan<- interface{}) Setting {
	return func(ctx context.Context, t *Thumbs) {
		t.result = result
	}
}

// WithParams provides parameters this Thumbs will use for rendering
func WithParams(params *params.Parameters) Setting {
	return func(ctx context.Context, t *Thumbs) {
		t.params = params
	}
}

// WithAlphabet sets the alphabet to use for generating hints
func WithAlphabet(alphabet string) Setting {
	return func(ctx context.Context, t *Thumbs) {
		if alphabet != "" {
			t.alphabet = []rune(alphabet)
		}
	}
}

func newThumbs(
	ctx context.Context,
	initial image.Image,
	origin geom.Vec2,
	matches []Match,
	settings ...Setting,
) *Thumbs {
	ti := textinput.New()
	ti.Focus()
	ti.Width = 20
	ti.Prompt = ""

	t := &Thumbs{
		Lifetime:  util.NewLifetime(ctx),
		render:    taro.NewRenderer(),
		textInput: ti,
		alphabet:  defaultAlphabet,
		params:    params.New(),
		initial:   initial,
	}

	for _, setting := range settings {
		setting(t.Ctx(), t)
	}

	t.hints = AssignHints(t.alphabet, matches, origin)

	return t
}

func New(
	ctx context.Context,
	initial image.Image,
	origin geom.Vec2,
	matches []Match,
	settings ...Setting,
) *taro.Program {
	t := newThumbs(ctx, initial, origin, matches, settings...)
	return taro.New(t.Ctx(), t)
}
