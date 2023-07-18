package fuzzy

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/mux/stream"

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
	stream := stream.NewTea(
		ctx,
		ui{
			options: options,
			selected: "one",
			renderer: lipgloss.NewRenderer(
				nil,
				termenv.WithProfile(profile),
			),
		},
		geom.DEFAULT_SIZE,
	)

	return &Fuzzy{
		Terminal: screen.NewTerminal(ctx, stream, geom.DEFAULT_SIZE),
	}
}

type ui struct {
	options  []string
	selected string
	renderer *lipgloss.Renderer
}

var _ tea.Model = (*ui)(nil)

func (m ui) Init() tea.Cmd {
	return nil
}

func (m ui) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	return m, nil
}

func (m ui) View() string {
	basic := m.renderer.NewStyle().
		Background(lipgloss.Color("#20111B")).
		Foreground(lipgloss.Color("#D5CCBA")).
		Width(30)

	inactive := basic.Copy().Background(lipgloss.Color("#968C83"))
	active := basic.Copy().Background(lipgloss.Color("#EAA549"))

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

	return lipgloss.JoinVertical(lipgloss.Left,
		basic.Render("$ text"),
		lipgloss.JoinVertical(lipgloss.Left, options...),
	)
}
