package fuzzy

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/mux/stream"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/muesli/termenv"
)

type Fuzzy struct {
	*screen.Terminal
}

var _ mux.Screen = (*Fuzzy)(nil)

func (f *Fuzzy) Resize(size mux.Size) error {
	return nil
}

func NewFuzzy(ctx context.Context, profile termenv.Profile, options []string) *Fuzzy {
	renderer := lipgloss.NewRenderer(
		nil,
		termenv.WithProfile(profile),
	)

	stream := stream.NewTea(
		ctx,
		initialModel(renderer, options),
		geom.DEFAULT_SIZE,
	)

	return &Fuzzy{
		Terminal: screen.NewTerminal(ctx, stream, geom.DEFAULT_SIZE),
	}
}

type model struct {
	options   []string
	selected  string
	renderer  *lipgloss.Renderer
	textInput textinput.Model
}

func initialModel(renderer *lipgloss.Renderer, options []string) model {
	ti := textinput.New()
	ti.Focus()
	ti.CharLimit = 20
	ti.Width = 20
	ti.Prompt = ""
	ti.Placeholder = "fuzzy: projects"

	return model{
		options: options,
		// TODO(cfoust): 07/18/23 don't assume
		selected:  options[0],
		renderer:  renderer,
		textInput: ti,
	}
}

var _ tea.Model = (*model)(nil)

func (m model) Init() tea.Cmd {
	return textinput.Blink
}

func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmd tea.Cmd
	m.textInput, cmd = m.textInput.Update(msg)
	return m, cmd
}

func (m model) View() string {
	basic := m.renderer.NewStyle().
		Background(lipgloss.Color("#20111B")).
		Foreground(lipgloss.Color("#D5CCBA")).
		Width(20)

	inactive := basic.Copy().Background(lipgloss.Color("#968C83"))
	active := basic.Copy().
		Background(lipgloss.Color("#EAA549")).
		Foreground(lipgloss.Color("#20111B"))

	var options []string
	for _, option := range m.options {
		style := inactive

		if m.selected == option {
			style = active
		}

		options = append(options,
			style.Render(option),
		)
	}

	m.textInput.Cursor.Style = m.renderer.NewStyle().
		Background(lipgloss.Color("#EAA549"))

	return lipgloss.JoinVertical(lipgloss.Left,
		basic.Render(m.textInput.View()),
		lipgloss.JoinVertical(lipgloss.Left, options...),
	)
}
