package text

import (
	"context"

	"github.com/cfoust/cy/pkg/anim"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/params"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
)

type Text struct {
	util.Lifetime
	anim   *taro.Program
	params *params.Parameters

	result chan<- interface{}
	size   geom.Vec2

	// Whether Text should render the input above or below the prompt.
	isUp      bool
	render    *taro.Renderer
	prompt    string
	textInput textinput.Model

	// Whether to render at `location` instead of filling the boundaries of
	// the screen.
	isInline bool
	location geom.Vec2

	// Whether to accept only a single character instead of full text input.
	isSingle bool
}

var _ taro.Model = (*Text)(nil)

func (t *Text) quit() (taro.Model, tea.Cmd) {
	return t, tea.Batch(
		func() tea.Msg {
			t.Cancel()
			return nil
		},
		tea.Quit,
	)
}

func (t *Text) Init() taro.Cmd {
	cmds := []taro.Cmd{
		textinput.Blink,
	}

	if t.anim != nil {
		w := taro.NewWatcher(t.Ctx(), t.anim)
		cmds = append(cmds, w.Wait())
	}

	return tea.Batch(cmds...)
}

type AcceptedEvent struct {
	Text string
}


type Setting func(context.Context, *Text)

func WithAnimation(image image.Image, creator anim.Creator) Setting {
	return func(ctx context.Context, t *Text) {
		t.anim = anim.NewAnimator(
			ctx,
			creator(),
			image,
			23,
		)
	}
}

func WithResult(result chan<- interface{}) Setting {
	return func(ctx context.Context, t *Text) {
		t.result = result
	}
}

func WithReverse(ctx context.Context, t *Text) {
	t.isUp = false
}

// WithInline displays Text as a small window at the provided location on the
// screen.
func WithInline(location, size geom.Vec2) Setting {
	return func(ctx context.Context, t *Text) {
		t.isInline = true
		t.location = location
		t.isUp = t.location.R > (size.R / 2)
	}
}

// WithPrompt controls the prompt text shown below (or above) the text input.
func WithPrompt(prompt string) Setting {
	return func(ctx context.Context, t *Text) {
		t.prompt = prompt
	}
}

// WithPlaceholder controls the string that is shown when the input is empty.
func WithPlaceholder(placeholder string) Setting {
	return func(ctx context.Context, t *Text) {
		t.textInput.Placeholder = placeholder
	}
}

// WithPreset prefills the text input with a string.
func WithPreset(preset string) Setting {
	return func(ctx context.Context, t *Text) {
		t.textInput.SetValue(preset)
	}
}

// WithParams provides parameters this Text will use for rendering.
func WithParams(params *params.Parameters) Setting {
	return func(ctx context.Context, t *Text) {
		t.params = params
	}
}

// WithSingle configures the text input to accept only a single character.
func WithSingle(ctx context.Context, t *Text) {
	t.isSingle = true
}

func newText(
	ctx context.Context,
	settings ...Setting,
) *Text {
	ti := textinput.New()
	ti.Focus()
	ti.Width = 20
	ti.Prompt = ""

	t := &Text{
		Lifetime:  util.NewLifetime(ctx),
		render:    taro.NewRenderer(),
		textInput: ti,
		isUp:      true,
	}

	for _, setting := range settings {
		setting(t.Ctx(), t)
	}
	return t
}

func New(
	ctx context.Context,
	settings ...Setting,
) *taro.Program {
	t := newText(ctx, settings...)
	return taro.New(t.Ctx(), t)
}
