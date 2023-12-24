package placeholder

import (
	"context"
	_ "embed"

	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/glamour"
)

//go:embed example.md
var EXAMPLE_MD string

// Renders some placeholder markdown text. Just used in stories.
type Markdown struct {
	render *taro.Renderer
}

var _ taro.Model = (*Markdown)(nil)

func (s *Markdown) Init() tea.Cmd {
	return nil
}

func (s *Markdown) View(state *tty.State) {
	text, _ := glamour.Render(EXAMPLE_MD, "dark")
	s.render.RenderAt(
		state.Image,
		0,
		0,
		text,
	)
}

func (s *Markdown) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	return s, nil
}

func New(ctx context.Context) mux.Screen {
	return taro.New(ctx, &Markdown{
		render: taro.NewRenderer(),
	})
}
