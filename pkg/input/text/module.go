package text

import (
	"context"

	"github.com/cfoust/cy/pkg/anim"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
)

type Text struct {
	util.Lifetime
	anim *taro.Program

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

func (t *Text) acceptText() taro.Cmd {
	return func() taro.Msg {
		return taro.PublishMsg{
			Msg: AcceptedEvent{
				Text: t.textInput.Value(),
			},
		}
	}
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

func newText(
	ctx context.Context,
	settings ...Setting,
) *Text {
	ti := textinput.New()
	ti.Focus()
	ti.CharLimit = 20
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
