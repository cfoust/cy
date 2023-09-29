package fuzzy

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type Fuzzy struct {
	util.Lifetime
	result   chan<- interface{}
	location geom.Vec2
	size     geom.Vec2

	render    *taro.Renderer
	textInput textinput.Model

	options  []Option
	filtered []Option
	selected int
	pattern  string
}

var _ taro.Model = (*Fuzzy)(nil)

func (f *Fuzzy) quit() (taro.Model, tea.Cmd) {
	return f, tea.Quit
}

func (f *Fuzzy) Init() taro.Cmd {
	return textinput.Blink
}

func (f *Fuzzy) getOptions() []Option {
	if len(f.pattern) > 0 {
		return f.filtered
	}
	return f.options
}

func (f *Fuzzy) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	var cmds []tea.Cmd
	var cmd tea.Cmd

	switch msg := msg.(type) {
	case matchResult:
		f.filtered = msg.Filtered
		f.selected = geom.Max(geom.Min(f.selected, len(f.getOptions())-1), 0)
		return f, nil
	case tea.WindowSizeMsg:
		f.size = geom.Size{
			R: msg.Height,
			C: msg.Width,
		}
	case taro.KeyMsg:
		switch msg.Type {
		case taro.KeyEsc, taro.KeyCtrlC:
			f.result <- nil
			return f.quit()
		case taro.KeyUp, taro.KeyCtrlK:
			f.selected = geom.Max(f.selected-1, 0)
			return f, nil
		case taro.KeyDown, taro.KeyCtrlJ:
			f.selected = geom.Min(f.selected+1, len(f.getOptions())-1)
			return f, nil
		case taro.KeyEnter:
			if f.selected < len(f.getOptions()) {
				option := f.getOptions()[f.selected]
				f.result <- option.Result
			} else {
				f.result <- nil
			}
			return f.quit()
		}
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

func (f *Fuzzy) View(state *tty.State) {
	basic := f.render.NewStyle().
		Background(lipgloss.Color("#20111B")).
		Foreground(lipgloss.Color("#D5CCBA")).
		Width(20)

	inactive := basic.Copy().Background(lipgloss.Color("#968C83"))
	active := basic.Copy().
		Background(lipgloss.Color("#EAA549")).
		Foreground(lipgloss.Color("#20111B"))

	// TODO(cfoust): 09/20/23 just use slices.Reverse in go 1.21
	var lines []string
	shouldInvert := f.location.R > (f.size.R / 2)

	// first, the options
	for i, match := range f.getOptions() {
		var rendered string
		if f.selected == i {
			rendered = active.Render(match.Text)
		} else {
			rendered = inactive.Render(match.Text)
		}

		if shouldInvert {
			lines = append([]string{rendered}, lines...)
		} else {
			lines = append(lines, rendered)
		}
	}

	// then the text input
	input := basic.Render(f.textInput.View())
	if shouldInvert {
		lines = append(lines, input)
	} else {
		lines = append([]string{input}, lines...)
	}

	f.textInput.Cursor.Style = f.render.NewStyle().
		Background(lipgloss.Color("#EAA549"))

	output := lipgloss.JoinVertical(lipgloss.Left, lines...)

	offset := 0
	if shouldInvert {
		offset += lipgloss.Height(output) - 1
	}

	f.render.RenderAt(
		state,
		geom.Max(f.location.R-offset, 0),
		f.location.C,
		output,
	)

	// the text input provides its own cursor
	state.CursorVisible = false
}

func NewFuzzy(
	ctx context.Context,
	info screen.RenderContext,
	options []Option,
	location geom.Vec2,
	result chan<- interface{},
) *taro.Program {
	ti := textinput.New()
	ti.Focus()
	ti.CharLimit = 20
	ti.Width = 20
	ti.Prompt = ""

	return taro.New(ctx, &Fuzzy{
		render:    taro.NewRenderer(),
		result:    result,
		location:  location,
		options:   options,
		selected:  0,
		textInput: ti,
	})
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
