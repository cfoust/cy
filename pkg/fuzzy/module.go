package fuzzy

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/mux/stream"

	tea "github.com/charmbracelet/bubbletea"
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

func (m ui) View() string {
	return "this is a test"
}
