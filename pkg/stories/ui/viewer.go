package ui

import (
	"context"
	"math"
	"time"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/keys"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/stories"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/rs/zerolog/log"
)

const MOUSE_STEP_DELAY = 15 * time.Millisecond

// CursorHandler receives cursor position updates during story
// playback, used for recording cursor events to cast files.
type CursorHandler func(r, c int, drag bool)

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

	mousePos      geom.Vec2
	hasMousePos   bool
	cursorHandler CursorHandler
	// The offset of the story content within the viewer, used
	// to translate story-relative cursor positions to
	// viewport-absolute positions for cast recording.
	storyOffset geom.Vec2
}

var _ taro.Model = (*Viewer)(nil)

func (v *Viewer) Init() tea.Cmd {
	return tea.Batch(
		v.sendInputs,
		taro.WaitScreens(v.Ctx(), v.screen, v.keys),
	)
}

type loadedScreen struct {
	screen   mux.Screen
	keys     mux.Screen
	lifetime util.Lifetime
	capture  *tty.State
}

type reloadScreen struct{}

// interpolatePath returns intermediate positions from src to dst,
// producing a smooth path for mouse cursor animation.
func interpolatePath(
	src, dst geom.Vec2,
) []geom.Vec2 {
	dr := dst.R - src.R
	dc := dst.C - src.C
	steps := int(math.Max(
		math.Abs(float64(dr)),
		math.Abs(float64(dc)),
	))
	if steps == 0 {
		return nil
	}

	var path []geom.Vec2
	for i := 1; i <= steps; i++ {
		t := float64(i) / float64(steps)
		path = append(path, geom.Vec2{
			R: src.R + int(math.Round(float64(dr)*t)),
			C: src.C + int(math.Round(float64(dc)*t)),
		})
	}
	return path
}

// If the story includes any inputs, cycle through them and
// reload the screen when they're done
func (v *Viewer) sendInputs() tea.Msg {
	screen := v.screen
	keyDisplay := v.keys
	inputs := v.story.Config.Input
	if screen == nil || len(inputs) == 0 {
		return nil
	}

	emitCursor := func(r, c int, drag bool) {
		if v.cursorHandler != nil {
			v.cursorHandler(
				r+v.storyOffset.R,
				c+v.storyOffset.C,
				drag,
			)
		}
	}

	sendMouse := func(m keys.Mouse) {
		screen.Send(taro.MouseMsg(m))
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
		case stories.MouseMoveEvent:
			path := interpolatePath(
				v.mousePos,
				input.Vec2,
			)
			for _, pos := range path {
				v.mousePos = pos
				v.hasMousePos = true
				sendMouse(keys.Mouse{
					Vec2: pos,
					Type: keys.MouseMotion,
				})
				time.Sleep(MOUSE_STEP_DELAY)
			}
			if len(path) == 0 {
				v.mousePos = input.Vec2
				v.hasMousePos = true
			}
			// Only record the final destination; the JS
			// player interpolates smoothly between endpoints.
			emitCursor(v.mousePos.R, v.mousePos.C, false)
			return
		case stories.MouseClickEvent:
			sendMouse(keys.Mouse{
				Vec2:   v.mousePos,
				Type:   keys.MousePress,
				Button: input.Button,
				Down:   true,
			})
			emitCursor(v.mousePos.R, v.mousePos.C, false)
			time.Sleep(50 * time.Millisecond)
			sendMouse(keys.Mouse{
				Vec2:   v.mousePos,
				Type:   keys.MousePress,
				Button: input.Button,
				Down:   false,
			})
			return
		case stories.MouseDragEvent:
			// Press at current position
			sendMouse(keys.Mouse{
				Vec2:   v.mousePos,
				Type:   keys.MousePress,
				Button: keys.MouseLeft,
				Down:   true,
			})
			emitCursor(v.mousePos.R, v.mousePos.C, true)
			time.Sleep(MOUSE_STEP_DELAY)

			// Move to destination with button held
			path := interpolatePath(
				v.mousePos,
				input.Vec2,
			)
			for _, pos := range path {
				v.mousePos = pos
				v.hasMousePos = true
				sendMouse(keys.Mouse{
					Vec2: pos,
					Type: keys.MouseMotion,
					Down: true,
				})
				time.Sleep(MOUSE_STEP_DELAY)
			}
			if len(path) == 0 {
				v.mousePos = input.Vec2
				v.hasMousePos = true
			}
			emitCursor(v.mousePos.R, v.mousePos.C, true)

			// Release
			sendMouse(keys.Mouse{
				Vec2:   v.mousePos,
				Type:   keys.MousePress,
				Button: keys.MouseLeft,
				Down:   false,
			})
			// End of drag
			emitCursor(v.mousePos.R, v.mousePos.C, false)
			return
		case stories.MouseScrollEvent:
			button := keys.MouseWheelDown
			if input.Up {
				button = keys.MouseWheelUp
			}
			sendMouse(keys.Mouse{
				Vec2:   v.mousePos,
				Type:   keys.MousePress,
				Button: button,
				Down:   true,
			})
			emitCursor(v.mousePos.R, v.mousePos.C, false)
			return
		}

		stories.Send(screen, input)
		stories.Send(keyDisplay, input)
	}

	for _, input := range inputs {
		handleInput(input)
	}

	return reloadScreen{}
}

func (v *Viewer) initScreen() (loadedScreen, error) {
	lifetime := util.NewLifetime(v.Ctx())
	screen, err := v.story.Init(lifetime.Ctx())
	if err != nil {
		lifetime.Cancel()
		return loadedScreen{}, err
	}

	config := v.story.Config
	if !config.Size.IsZero() {
		_ = screen.Resize(config.Size)
	}

	msg := loadedScreen{
		screen:   screen,
		keys:     NewKeys(lifetime.Ctx()),
		lifetime: lifetime,
	}

	if config.IsSnapshot {
		msg.capture = screen.State()
	}

	return msg, nil
}

func (v *Viewer) applyScreen(msg loadedScreen) {
	if v.screen != nil {
		v.screenLifetime.Cancel()
	}

	v.screen = msg.screen
	v.keys = msg.keys
	v.screenLifetime = msg.lifetime
	v.capture = msg.capture
	v.hasMousePos = false
}

func (v *Viewer) showKeys() bool {
	config := v.story.Config
	return config.Size.IsZero() && len(config.Input) > 0
}

func (v *Viewer) View(state *tty.State) {
	if v.screen == nil {
		state.CursorVisible = false
		return
	}

	size := state.Image.Size()

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
		v.storyOffset = storyPos
		tty.Copy(storyPos, state, contents)
		v.renderCursor(state, storyPos)
		return
	}

	v.storyOffset = geom.Size{C: KEY_COLUMNS}
	tty.Copy(geom.Size{C: KEY_COLUMNS}, state, contents)
	image.Copy(geom.Size{}, state.Image, v.keys.State().Image)
	v.renderCursor(state, geom.Size{C: KEY_COLUMNS})
}

func (v *Viewer) renderCursor(
	state *tty.State,
	offset geom.Vec2,
) {
	// When recording for cast export, the JS overlay handles
	// the cursor; skip the in-terminal one.
	if !v.hasMousePos || v.cursorHandler != nil {
		return
	}

	absR := offset.R + v.mousePos.R
	absC := offset.C + v.mousePos.C
	imgSize := state.Image.Size()

	if absR < 0 || absR >= imgSize.R ||
		absC < 0 || absC >= imgSize.C {
		return
	}

	state.Image[absR][absC].BG = emu.ANSIColor(5)
}

func (v *Viewer) resize(size geom.Size) {
	config := v.story.Config
	if !v.showKeys() {
		if !config.Size.IsZero() {
			return
		}

		_ = v.screen.Resize(size)
		return
	}

	keySize := geom.Size{
		R: v.size.R,
		C: KEY_COLUMNS,
	}
	_ = v.keys.Resize(keySize)
	_ = v.screen.Resize(geom.Size{
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

		return v, func() tea.Msg {
			loaded, err := v.initScreen()
			if err != nil {
				log.Error().Err(err).Msgf(
					"failed loading story",
				)
				return nil
			}
			return loaded
		}
	case loadedScreen:
		v.applyScreen(msg)
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

type ViewerOption func(*Viewer)

func WithCursorHandler(h CursorHandler) ViewerOption {
	return func(v *Viewer) {
		v.cursorHandler = h
	}
}

func NewViewer(
	ctx context.Context,
	story stories.Story,
	shouldLoop bool,
	opts ...ViewerOption,
) (*taro.Program, error) {
	viewer := &Viewer{
		Lifetime:   util.NewLifetime(ctx),
		story:      story,
		shouldLoop: shouldLoop,
	}

	for _, opt := range opts {
		opt(viewer)
	}

	loaded, err := viewer.initScreen()
	if err != nil {
		viewer.Cancel()
		return nil, err
	}
	viewer.applyScreen(loaded)

	program := taro.New(ctx, viewer)
	return program, nil
}
