package fuzzy

import (
	"context"
	"strings"

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

func NewFuzzy(ctx context.Context, options []string) *Fuzzy {
	stream := stream.NewTea(
		ctx,
		ui{options: options},
		geom.DEFAULT_SIZE,
	)

	return &Fuzzy{
		Terminal: screen.NewTerminal(ctx, stream, geom.DEFAULT_SIZE),
	}
}

type ui struct {
	options []string
}

var _ tea.Model = (*ui)(nil)

func (m ui) Init() tea.Cmd {
	return nil
}

func (m ui) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	return m, nil
}

var ()

func (m ui) View() string {
	var b strings.Builder
	renderer := lipgloss.NewRenderer(&b, termenv.WithProfile(termenv.TrueColor))
	titleStyle := renderer.NewStyle().
		MarginLeft(1).
		MarginRight(5).
		Padding(0, 1).
		Italic(true).
		Foreground(lipgloss.Color("#FFF7DB")).
		Background(lipgloss.Color("#F25D94"))

	return titleStyle.Render("this is a test")
}
