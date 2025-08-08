package placeholder

import (
	"context"
	_ "embed"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	S "github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/xo/terminfo"

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

func FromMarkdown(ctx context.Context, text string) mux.Screen {
	rendered, _ := glamour.Render(text, "dark")
	term := emu.New()
	term.Write([]byte(emu.LineFeedMode))

	// Hide the cursor
	info, err := terminfo.Load("xterm-256color")
	if err != nil {
		return nil
	}
	info.Fprintf(term, terminfo.CursorInvisible)

	term.Write([]byte(rendered))
	return S.NewStaticTerminal(ctx, term)
}
