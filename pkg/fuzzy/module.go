package fuzzy

import (
	"context"

	"github.com/cfoust/cy/pkg/anim"
	"github.com/cfoust/cy/pkg/fuzzy/preview"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
)

type Fuzzy struct {
	util.Lifetime
	anim *taro.Program

	result chan<- interface{}
	size   geom.Vec2

	render    *taro.Renderer
	textInput textinput.Model

	// Don't allow Fuzzy to quit or the user to choose anything
	isSticky bool

	// Whether Fuzzy should display options above or below the input.
	isUp bool

	// Whether to render at `location` instead of filling the boundaries of
	// the screen.
	isInline bool
	location geom.Vec2

	// before the user has done anything, we don't show the preview window
	haveMoved bool

	options  []Option
	filtered []Option
	selected int
	pattern  string

	// shown before the number of items
	prompt string

	// headers for the table
	headers []string

	tree    *tree.Tree
	client  *server.Client
	preview preview.Preview
}

var _ taro.Model = (*Fuzzy)(nil)

func (f *Fuzzy) quit() (taro.Model, tea.Cmd) {
	if f.isSticky {
		return f, nil
	}
	return f, tea.Batch(
		func() tea.Msg {
			f.Cancel()
			return nil
		},
		tea.Quit,
	)
}

func (f *Fuzzy) Init() taro.Cmd {
	cmds := []taro.Cmd{
		textinput.Blink,
	}

	if f.anim != nil {
		cmds = append(cmds, taro.WaitScreens(f.Ctx(), f.anim))
	}

	return tea.Batch(cmds...)
}

func (f *Fuzzy) getOptions() []Option {
	if len(f.pattern) > 0 {
		return f.filtered
	}
	return f.options
}

type SelectedEvent struct {
	Option Option
}

func (f *Fuzzy) setSelected(index int) {
	f.selected = geom.Max(
		0,
		geom.Clamp(index, 0, len(f.getOptions())-1),
	)
}

func (f *Fuzzy) emitOption() taro.Cmd {
	if len(f.getOptions()) == 0 {
		return nil
	}

	return func() taro.Msg {
		return taro.PublishMsg{
			Msg: SelectedEvent{
				Option: f.getOptions()[f.selected],
			},
		}
	}
}

// Previews are just screens. Background is transparent by default.

func (f *Fuzzy) handlePreview() taro.Cmd {
	options := f.getOptions()
	if len(options) == 0 {
		return nil
	}

	option := options[f.selected]
	if option.Preview == nil {
		return nil
	}

	p := preview.New(option.Preview, f.tree, f.client)
	if p == nil {
		return nil
	}

	return p.Init(f.Ctx())
}

func (f *Fuzzy) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	var cmds []tea.Cmd
	var cmd tea.Cmd

	switch msg := msg.(type) {
	case taro.ScreenUpdate:
		return f, taro.WaitScreens(f.Ctx(), f.anim)
	case matchResult:
		f.filtered = msg.Filtered
		f.setSelected(f.selected)
		return f, f.emitOption()
	case tea.WindowSizeMsg:
		size := geom.Size{
			R: msg.Height,
			C: msg.Width,
		}
		if f.anim != nil {
			f.anim.Resize(size)
		}
		f.size = size
		f.location = geom.Vec2{
			R: geom.Clamp(f.location.R, 0, size.R-1),
			C: geom.Clamp(f.location.C, 0, size.C-1),
		}
	case taro.KeyMsg:
		switch msg.Type {
		case taro.KeyEsc, taro.KeyCtrlC:
			if f.result != nil {
				f.result <- nil
			}
			return f.quit()
		case taro.KeyDown, taro.KeyCtrlJ, taro.KeyUp, taro.KeyCtrlK:
			f.haveMoved = true
			upwards := false
			switch msg.Type {
			case taro.KeyUp, taro.KeyCtrlK:
				upwards = true
			}
			if f.isUp {
				upwards = !upwards
			}

			delta := -1
			if !upwards {
				delta = 1
			}

			f.setSelected(f.selected + delta)
			return f, tea.Batch(
				f.handlePreview(),
				f.emitOption(),
			)
		case taro.KeyEnter:
			if f.isSticky {
				return f, nil
			}

			if f.selected >= 0 && f.selected < len(f.getOptions()) {
				option := f.getOptions()[f.selected]
				f.result <- option.Result
			} else {
				f.result <- nil
			}
			return f.quit()
		}
	default:
		if f.preview == nil {
			break
		}

		f.preview, cmd = f.preview.Update(msg)
		cmds = append(cmds, cmd)
	}

	inputMsg := msg
	// We need to translate taro.KeyMsg to tea.KeyMsg (for now)
	if key, ok := msg.(taro.KeyMsg); ok {
		inputMsg = key.ToTea()
	}
	f.textInput, cmd = f.textInput.Update(inputMsg)
	cmds = append(cmds, cmd)

	value := f.textInput.Value()
	if f.pattern != value {
		f.pattern = value
		cmds = append(cmds, queryOptions(f.options, value))
	}

	return f, tea.Batch(cmds...)
}

type Setting func(context.Context, *Fuzzy)

func WithAnimation(image image.Image) Setting {
	return func(ctx context.Context, f *Fuzzy) {
		f.anim = anim.NewAnimator(
			ctx,
			anim.RandomAnimation(),
			image,
			23,
		)
	}
}

// Don't allow Fuzzy to quit.
func WithSticky(ctx context.Context, f *Fuzzy) {
	f.isSticky = true
}

// If both of these are provided, Fuzzy can show previews for panes.
func WithNodes(t *tree.Tree, client *server.Client) Setting {
	return func(ctx context.Context, f *Fuzzy) {
		f.tree = t
		f.client = client
	}
}

func WithResult(result chan<- interface{}) Setting {
	return func(ctx context.Context, f *Fuzzy) {
		f.result = result
	}
}

// Displays Fuzzy as a small window at this location on the screen.
func WithInline(location, size geom.Vec2) Setting {
	return func(ctx context.Context, f *Fuzzy) {
		f.isInline = true
		f.location = location
		f.isUp = f.location.R > (size.R / 2)
	}
}

func WithReverse(ctx context.Context, f *Fuzzy) {
	f.isUp = false
}

func WithPrompt(prompt string) Setting {
	return func(ctx context.Context, f *Fuzzy) {
		f.prompt = prompt
	}
}

func WithHeaders(headers ...string) Setting {
	return func(ctx context.Context, f *Fuzzy) {
		f.headers = headers
	}
}

func newFuzzy(
	ctx context.Context,
	options []Option,
	settings ...Setting,
) *Fuzzy {
	ti := textinput.New()
	ti.Focus()
	ti.CharLimit = 20
	ti.Width = 20
	ti.Prompt = ""

	f := &Fuzzy{
		Lifetime:  util.NewLifetime(ctx),
		render:    taro.NewRenderer(),
		options:   options,
		selected:  0,
		textInput: ti,
		isUp:      true,
	}

	for _, setting := range settings {
		setting(f.Ctx(), f)
	}
	return f
}

func New(
	ctx context.Context,
	options []Option,
	settings ...Setting,
) *taro.Program {
	f := newFuzzy(ctx, options, settings...)
	return taro.New(f.Ctx(), f)
}

type matchResult struct {
	Filtered []Option
}

func queryOptions(options []Option, pattern string) tea.Cmd {
	return func() tea.Msg {
		return matchResult{
			Filtered: Filter(options, pattern),
		}
	}
}
