package replay

import (
	"context"
	"time"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux/screen/replay/player"
	"github.com/cfoust/cy/pkg/sessions/search"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
)

type Replay struct {
	render *taro.Renderer
	binds  *bind.Engine[bind.Action]

	// whether Replay will actually quit itself
	preventExit bool

	// the size of the client, but minus one row
	// we don't want to obscure content
	viewport geom.Size

	mode Mode

	isPlaying    bool
	playbackRate int
	currentTime  time.Time

	// Whether moving in time should skip inactivity
	skipInactivity bool

	// The location of Replay in time
	location search.Address
	player   *player.Player

	// `offset` is used in two different ways depending on whether the
	// terminal is on the alt screen:
	// * On the main screen: the `R` field refers to the line in history
	//   and the `C` refers to a column in that line that the top-left cell
	//   of the screen contains.
	// * On the alt screen: the [R, C] offset of the viewport relative to
	//   the top-left corner of the underlying terminal.
	offset, minOffset, maxOffset geom.Vec2

	// The cursor's position relative to the viewport.
	cursor geom.Vec2
	// Used to mimic the behavior in text editors wherein moving the cursor
	// up and down "sticks" to a certain column index wherever possible
	desiredCol int

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

func (r *Replay) getTerminalCursor() geom.Vec2 {
	cursor := r.player.Cursor()
	return geom.Vec2{
		R: cursor.Y,
		C: cursor.X,
	}
}

func (r *Replay) getTerminalSize() geom.Vec2 {
	cols, rows := r.player.Size()
	return geom.Vec2{
		R: rows,
		C: cols,
	}
}

// Get the glyphs for a row in term space.
func (r *Replay) getLine(row int) emu.Line {
	screen := r.player.Screen()
	history := r.player.History()

	// Handle out-of-bounds lines
	clamped := geom.Clamp(row, -len(history), r.getTerminalSize().R-1)
	if clamped != row {
		return nil
	}

	var line emu.Line
	if row < 0 {
		line = history[len(history)+row]
	} else {
		line = screen[row]
	}

	return line
}

func (r *Replay) exitCopyMode() {
	r.mode = ModeTime
	r.isSelecting = false
	termCursor := r.getTerminalCursor()
	r.centerPoint(termCursor)
	r.cursor = r.termToViewport(termCursor)
	r.desiredCol = r.cursor.C
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
		render:         taro.NewRenderer(),
		player:         player,
		searchInput:    ti,
		playbackRate:   1,
		binds:          binds,
		searchProgress: make(chan int),
		skipInactivity: true,
	}
	m.gotoIndex(-1, -1)
	return m
}

type ReplayOption func(r *Replay)

func WithNoQuit(r *Replay) {
	r.preventExit = true
}

func New(
	ctx context.Context,
	player *player.Player,
	replayBinds *bind.BindScope,
	options ...ReplayOption,
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
