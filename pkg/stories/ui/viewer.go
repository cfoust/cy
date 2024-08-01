package ui

import (
	"context"
	"time"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/stories"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/rs/zerolog/log"
)

// A Viewer shows a single story.
type Viewer struct {
	util.Lifetime
	size    geom.Vec2
	story   stories.Story
	capture *tty.State

	screen         mux.Screen
	keys           mux.Screen
	screenLifetime util.Lifetime
	// Whether the viewer should reload the screen and loop, or just quit
	shouldLoop bool
}

var _ taro.Model = (*Viewer)(nil)

func (v *Viewer) Init() tea.Cmd {
	return v.loadStory()
}

type loadedScreen struct {
	screen   mux.Screen
	keys     mux.Screen
	lifetime util.Lifetime
	capture  *tty.State
}

type reloadScreen struct{}

// If the story includes any inputs, cycle through them and
// reload the screen when they're done
func (v *Viewer) sendInputs() tea.Msg {
	screen := v.screen
	keys := v.keys
	inputs := v.story.Config.Input
	if screen == nil || len(inputs) == 0 {
		return nil
	}

	var handleInput func(input interface{})
	handleInput = func(input interface{}) {
		switch input := input.(type) {
		case stories.WaitEvent:
			time.Sleep(input.Duration)
			return
		case []interface{}:
			for _, input := range input {
				handleInput(input)
			}
			return
		}

		stories.Send(screen, input)
		stories.Send(keys, input)
	}

	for _, input := range inputs {
		handleInput(input)
	}

	return reloadScreen{}
}

func (v *Viewer) loadStory() tea.Cmd {
	story := v.story
	config := story.Config

	return func() tea.Msg {
		lifetime := util.NewLifetime(v.Ctx())
		screen, err := story.Init(lifetime.Ctx())
		if err != nil {
			log.Error().Err(err).Msgf("failed loading story")
			return nil
		}

		if !config.Size.IsZero() {
			screen.Resize(config.Size)
		}

		keys := NewKeys(lifetime.Ctx())
		msg := loadedScreen{
			lifetime: lifetime,
			screen:   screen,
			keys:     keys,
		}

		if config.IsSnapshot {
			msg.capture = screen.State()
		}

		return msg
	}

}

func (v *Viewer) showKeys() bool {
	config := v.story.Config
	return config.Size.IsZero() && len(config.Input) > 0
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

	if !v.showKeys() {
		tty.Copy(storyPos, state, contents)
		return
	}

	tty.Copy(geom.Size{C: KEY_COLUMNS}, state, contents)
	image.Copy(geom.Size{}, state.Image, v.keys.State().Image)
}

func (v *Viewer) resize(size geom.Size) {
	config := v.story.Config
	if !v.showKeys() {
		if !config.Size.IsZero() {
			return
		}

		v.screen.Resize(size)
		return
	}

	keySize := geom.Size{
		R: v.size.R,
		C: KEY_COLUMNS,
	}
	v.keys.Resize(keySize)
	v.screen.Resize(geom.Size{
		R: v.size.R,
		C: geom.Max(0, v.size.C-keySize.C),
	})
}

func (v *Viewer) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case reloadScreen:
		if !v.shouldLoop {
			return v, tea.Quit
		}

		return v, v.loadStory()
	case loadedScreen:
		if v.screen != nil {
			v.screenLifetime.Cancel()
			v.screen = nil
		}

		v.capture = msg.capture
		v.screen = msg.screen
		v.keys = msg.keys
		v.screenLifetime = msg.lifetime
		v.resize(v.size)

		return v, tea.Batch(
			v.sendInputs,
			taro.WaitScreens(v.Ctx(), v.screen, v.keys),
		)
	case tea.WindowSizeMsg:
		size := geom.Size{
			R: msg.Height,
			C: msg.Width,
		}
		v.size = size

		if v.screen != nil {
			v.resize(v.size)
		}

		return v, nil
	case taro.ScreenUpdate:
		return v, taro.WaitScreens(v.Ctx(), v.screen, v.keys)
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
	shouldLoop bool,
) *taro.Program {
	viewer := &Viewer{
		Lifetime:   util.NewLifetime(ctx),
		story:      story,
		shouldLoop: shouldLoop,
	}

	program := taro.New(ctx, viewer)

	return program
}
