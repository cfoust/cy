package stories

import (
	"context"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	tea "github.com/charmbracelet/bubbletea"
)

// A Viewer shows a single story.
type Viewer struct {
	util.Lifetime
	config  Config
	render  *taro.Renderer
	screen  mux.Screen
	capture *tty.State
}

var _ taro.Model = (*Viewer)(nil)

func (v *Viewer) Init() tea.Cmd {
	return taro.WaitScreens(v.Ctx(), v.screen)
}

func (v *Viewer) View(state *tty.State) {
	// Show an obvious background
	size := state.Image.Size()
	for row := 0; row < size.R; row++ {
		for col := 0; col < size.C; col++ {
			glyph := emu.EmptyGlyph()
			glyph.FG = 8
			glyph.Char = '-'
			state.Image[row][col] = glyph
		}
	}

	contents := v.screen.State()
	if v.capture != nil {
		contents = v.capture
	}

	storySize := contents.Image.Size()
	storyPos := geom.Vec2{
		R: (size.R / 2) - (storySize.R / 2),
		C: (size.C / 2) - (storySize.C / 2),
	}

	state.Image.Clear(geom.Rect{
		Position: storyPos,
		Size:     storySize,
	})
	tty.Copy(storyPos, state, contents)
	state.CursorVisible = contents.CursorVisible
}

func (v *Viewer) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		if !v.config.Size.IsZero() {
			return v, nil
		}
		v.screen.Resize(geom.Size{
			R: msg.Height,
			C: msg.Width,
		})
		return v, nil
	case taro.ScreenUpdate:
		return v, taro.WaitScreens(v.Ctx(), v.screen)
	case taro.KeyMsg:
		switch msg.String() {
		case "q":
			return v, tea.Quit
		}
	}

	return v, nil
}

func NewViewer(
	ctx context.Context,
	screen mux.Screen,
	config Config,
) *taro.Program {
	viewer := &Viewer{
		Lifetime: util.NewLifetime(ctx),
		render:   taro.NewRenderer(),
		config:   config,
		screen:   screen,
	}

	if config.IsSnapshot {
		viewer.capture = screen.State()
	}

	program := taro.New(ctx, viewer)
	return program
}
