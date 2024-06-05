package replay

import (
	"context"
	"time"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/replay/movement"
	"github.com/cfoust/cy/pkg/replay/player"
	"github.com/cfoust/cy/pkg/sessions/search"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
)

type Replay struct {
	*player.Player

	render *taro.Renderer
	binds  *bind.Engine[bind.Action]

	// Whether to show segmented command input and output. Only useful in
	// testing (for now?)
	showCommands bool

	// whether Replay will actually quit itself
	preventExit bool

	// whether the player is seeking
	isSeeking bool

	// the size of the client, but minus one row
	// we don't want to obscure content
	viewport geom.Size

	mode Mode

	// Replay allows you to browse the contents of the terminal screen in
	// two different ways:
	//   - By treating the screen as an image with arbitrary content that
	//   may not take the form of lines, such as when a full-screen
	//   application is open
	//   - By treating the screen as a text file with human-readable text,
	//   such as when a shell is running
	isSwapped bool

	isPlaying    bool
	playbackRate int
	currentTime  time.Time

	movement movement.Movement

	// Whether moving in time should skip inactivity
	skipInactivity bool

	// Whether the user has started selecting.
	isSelecting bool
	// The location in terminal space where the select began
	selectStart geom.Vec2

	isForward bool
	isWaiting bool
	// Whether no matches came back
	isEmpty         bool
	searchProgress  chan int
	progressPercent int
	searchInput     textinput.Model
	matches         []search.SearchResult

	// The last character the user jumped to
	jumpChar string
	// Whether that jump was forward
	wasJumpForward bool
	// Whether that jump was "to" or up until
	wasJumpTo bool
}

var _ taro.Model = (*Replay)(nil)

func (r *Replay) isCopyMode() bool {
	return r.mode == ModeCopy
}

func (r *Replay) enterCopyMode() {
	r.mode = ModeCopy
	r.movement.Snap()
}

func (r *Replay) getTerminalCursor() geom.Vec2 {
	cursor := r.Cursor()
	return geom.Vec2{
		R: cursor.R,
		C: cursor.C,
	}
}

func (r *Replay) swapScreen() {
	if !r.IsAltMode() {
		return
	}

	r.mode = ModeCopy
	r.isSwapped = !r.isSwapped
	r.isSelecting = false
	r.initializeMovement()
}

func (r *Replay) Init() tea.Cmd {
	return textinput.Blink
}

func newReplay(
	player *player.Player,
	binds *bind.Engine[bind.Action],
) *Replay {
	ti := textinput.New()
	ti.Focus()
	ti.CharLimit = 20
	ti.Width = 20
	ti.Prompt = ""
	m := &Replay{
		Player:         player,
		render:         taro.NewRenderer(),
		searchInput:    ti,
		playbackRate:   1,
		binds:          binds,
		searchProgress: make(chan int),
		skipInactivity: true,
	}
	m.Update(m.gotoIndex(-1, -1)())
	return m
}

type Option func(r *Replay)

func WithNoQuit(r *Replay) {
	r.preventExit = true
}

// WithCopyMode puts Replay immediately into copy mode.
func WithCopyMode(r *Replay) {
	r.enterCopyMode()
}

// WithFlow swaps to flow mode, if possible.
func WithFlow(r *Replay) {
	if !r.IsAltMode() {
		return
	}

	r.swapScreen()
}

// WithLocation attempts to move the cursor to `location`, which is a point in
// the coordinate space of the terminal's current mode.
func WithLocation(location geom.Vec2) Option {
	return func(r *Replay) {
		r.enterCopyMode()
		r.movement.Goto(location)
	}
}

func New(
	ctx context.Context,
	player *player.Player,
	replayBinds *bind.BindScope,
	options ...Option,
) *taro.Program {
	engine := bind.NewEngine[bind.Action]()
	engine.SetScopes(replayBinds)
	go engine.Poll(ctx)
	r := newReplay(player, engine)
	for _, option := range options {
		option(r)
	}
	program := taro.New(ctx, r)

	go func() {
		for {
			select {
			case <-ctx.Done():
				return
			case event := <-engine.Recv():
				if bindEvent, ok := event.(bind.BindEvent); ok {
					program.Publish(bindEvent)
				}
			}
		}
	}()

	return program
}
