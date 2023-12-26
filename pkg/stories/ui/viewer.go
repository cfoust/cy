package ui

import (
	"context"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/stories"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	tea "github.com/charmbracelet/bubbletea"
)

// A Viewer shows a single story.
type Viewer struct {
	util.Lifetime
	size    geom.Vec2
	story   stories.Story
	render  *taro.Renderer
	capture *tty.State

	screen         mux.Screen
	screenLifetime util.Lifetime
}

var _ taro.Model = (*Viewer)(nil)

func (v *Viewer) Init() tea.Cmd {
	//return taro.WaitScreens(v.Ctx(), v.screen)
	return v.loadStory()
}

type loadedScreen struct {
	screen   mux.Screen
	lifetime util.Lifetime
	capture  *tty.State
}

func (v *Viewer) loadStory() tea.Cmd {
	story := v.story

	//inputs := config.Input
	//if len(inputs) > 0 {
	//go func() {
	//for _, input := range inputs {
	//switch input := input.(type) {
	//case stories.WaitEvent:
	//time.Sleep(input.Duration)
	//continue
	//}

	//stories.Send(screen, input)
	//}
	//}()
	//}

	return func() tea.Msg {
		lifetime := util.NewLifetime(v.Ctx())
		screen, _ := story.Init(lifetime.Ctx())

		config := story.Config
		if !config.Size.IsZero() {
			screen.Resize(config.Size)
		}

		msg := loadedScreen{screen: screen, lifetime: lifetime}

		if config.IsSnapshot {
			msg.capture = screen.State()
		}

		return msg
	}
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

	if v.screen == nil {
		state.CursorVisible = false
		return
	}

	contents := v.screen.State()
	if v.capture != nil {
		contents = v.capture
	}

	storySize := contents.Image.Size()
	storyPos := size.Center(storySize)
	state.Image.Clear(geom.Rect{
		Position: storyPos,
		Size:     storySize,
	})
	tty.Copy(storyPos, state, contents)
	state.CursorVisible = contents.CursorVisible
}

func (v *Viewer) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case loadedScreen:
		v.capture = msg.capture
		v.screen = msg.screen

		if v.story.Config.Size.IsZero() {
			v.screen.Resize(v.size)
		}

		return v, taro.WaitScreens(v.Ctx(), v.screen)
	case tea.WindowSizeMsg:
		size := geom.Size{
			R: msg.Height,
			C: msg.Width,
		}
		v.size = size

		if v.screen != nil && v.story.Config.Size.IsZero() {
			v.screen.Resize(size)
		}

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
	story stories.Story,
) *taro.Program {
	viewer := &Viewer{
		Lifetime: util.NewLifetime(ctx),
		render:   taro.NewRenderer(),
		story:    story,
	}

	program := taro.New(ctx, viewer)

	return program
}
