package stories

import (
	"context"

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
	size := state.Image.Size()
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
		R: storyPos.R,
		C: storyPos.C,
		H: storySize.R,
		W: storySize.C,
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
