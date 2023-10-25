package replay

import (
	"context"
	"time"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/sessions/search"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
)

type Replay struct {
	render *taro.Renderer
	binds  *bind.Engine[bind.Action]

	// the size of the terminal
	terminal emu.Terminal

	viewport geom.Size

	isPlaying    bool
	playbackRate int
	currentTime  time.Time

	// The location of Replay in time
	// R: the index of the displayed event in `events`
	// C: the byte within it
	location search.Address
	events   []sessions.Event

	// Selection mode occurs when the user moves the cursor or scrolls the
	// window
	isSelectionMode bool

	// The offset of the viewport relative to the top-left corner of the
	// underlying terminal.
	//
	// offset.R is in the range
	// [-1 * number of scrollback lines, min(-(height of terminal - height of viewport), 0)]
	// positive indices mean the viewport is inside of the scrollback buffer
	// negative indices mean the viewport is viewing only part of the terminal's screen
	//
	// offset.C is in the range [0, max(width of terminal - width of viewport, 0)]
	//
	// For example:
	// * offset.R == 1: the viewport shows the first scrollback line
	offset, minOffset, maxOffset geom.Vec2

	// The cursor's position relative to the viewport.
	cursor geom.Vec2
	// Used to emulate the behavior in text editors wherein moving the
	// cursor up and down "sticks" to a certain column index wherever
	// possible
	desiredCol int

	isSearching bool

	isForward   bool
	isWaiting   bool
	searchInput textinput.Model
	matchIndex  int
	matches     []search.SearchResult
}

var _ taro.Model = (*Replay)(nil)

func (r *Replay) quit() (taro.Model, tea.Cmd) {
	return r, tea.Quit
}

func (r *Replay) getTerminalCursor() geom.Vec2 {
	cursor := r.terminal.Cursor()
	return geom.Vec2{
		R: cursor.Y,
		C: cursor.X,
	}
}

func (r *Replay) getTerminalSize() geom.Vec2 {
	cols, rows := r.terminal.Size()
	return geom.Vec2{
		R: rows,
		C: cols,
	}
}

// Get the glyphs for a row in term space.
func (r *Replay) getLine(row int) emu.Line {
	screen := r.terminal.Screen()
	history := r.terminal.History()

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

func (r *Replay) exitSelectionMode() {
	r.isSelectionMode = false
	termCursor := r.getTerminalCursor()
	r.center(termCursor)
	r.cursor = r.termToViewport(termCursor)
	r.desiredCol = r.cursor.C
}

func (r *Replay) Init() tea.Cmd {
	return textinput.Blink
}

func reverse[S ~[]E, E any](s S) {
	for i, j := 0, len(s)-1; i < j; i, j = i+1, j-1 {
		s[i], s[j] = s[j], s[i]
	}
}

func (r *Replay) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	viewport := r.viewport

	switch msg := msg.(type) {
	case PlaybackRateEvent:
		r.playbackRate = geom.Clamp(msg.Rate, -10, 10)
		if r.playbackRate == 0 {
			r.playbackRate = 1
		}
		return r, nil
	case PlaybackEvent:
		if !r.isPlaying {
			return r, nil
		}

		r.setTimeDelta(time.Duration(int64(time.Now().Sub(msg.Since)) * int64(r.playbackRate)))

		return r.scheduleUpdate()
	case tea.WindowSizeMsg:
		return r.setViewport(
			r.viewport,
			geom.Size{
				R: msg.Height,
				C: msg.Width,
			},
		)
	case SearchResultEvent:
		r.isWaiting = false

		// TODO(cfoust): 10/13/23 handle error

		matches := msg.results
		if len(matches) == 0 {
			r.matches = matches
			return r, nil
		}

		origin := msg.origin

		if msg.isForward {
			startIndex := 0
			for i, match := range matches {
				begin := match.Begin
				if begin.Index >= origin.Index && begin.Offset > origin.Offset {
					startIndex = i
					break
				}
			}

			matches = append(matches[startIndex:], matches[:startIndex]...)
		} else {
			reverse(matches)

			startIndex := 0
			for i, match := range matches {
				begin := match.Begin
				if begin.Index <= origin.Index && begin.Offset <= origin.Offset {
					startIndex = i
					break
				}
			}

			matches = append(matches[startIndex:], matches[:startIndex]...)
		}

		r.matches = matches

		if len(r.matches) > 0 {
			r.gotoMatch(0)
		}

		return r, nil
	}

	if r.isSearching {
		switch msg := msg.(type) {
		case ActionEvent:
			switch msg.Type {
			case ActionQuit:
				r.isSearching = false
				return r, nil
			}
		case taro.KeyMsg:
			switch msg.Type {
			case taro.KeyEnter:
				value := r.searchInput.Value()

				r.searchInput.Reset()
				r.isWaiting = true
				r.isSearching = false
				r.matches = make([]search.SearchResult, 0)

				location := r.location
				isForward := r.isForward
				events := r.events

				return r, func() tea.Msg {
					res, err := search.Search(events, value)
					return SearchResultEvent{
						isForward: isForward,
						origin:    location,
						results:   res,
						err:       err,
					}
				}
			}
		}
		var cmd tea.Cmd
		inputMsg := msg
		if key, ok := msg.(taro.KeyMsg); ok {
			inputMsg = key.ToTea()
		}
		r.searchInput, cmd = r.searchInput.Update(inputMsg)
		return r, cmd
	}

	// These events do not stop playback
	switch msg := msg.(type) {
	case ActionEvent:
		switch msg.Type {
		case ActionTimePlay:
			r.isPlaying = !r.isPlaying

			if r.isPlaying {
				r.exitSelectionMode()
				return r.scheduleUpdate()
			}

			return r, nil
		}
	case taro.KeyMsg:
		// Pass unmatched keys into the binding engine; because of how
		// text input works, :replay bindings have to be activated
		// selectively
		return r, func() tea.Msg {
			r.binds.InputMessage(msg)
			return nil
		}
	}

	// Every other event causes us to pause
	r.isPlaying = false

	switch msg := msg.(type) {
	case taro.MouseMsg:
		switch msg.Type {
		case taro.MouseWheelUp:
			r.setScroll(r.offset.R - 1)
		case taro.MouseWheelDown:
			r.setScroll(r.offset.R + 1)
		}
	case ActionEvent:
		switch msg.Type {
		case ActionQuit:
			if r.isSelectionMode {
				r.exitSelectionMode()
				return r, nil
			}

			return r.quit()
		case ActionBeginning:
			if r.isSelectionMode {
				r.moveCursorDelta(
					-r.viewportToTerm(r.cursor).R+r.minOffset.R,
					0,
				)
			} else {
				r.gotoIndex(0, -1)
			}
		case ActionEnd:
			if r.isSelectionMode {
				r.moveCursorDelta(
					(r.getTerminalSize().R-1)-r.viewportToTerm(r.cursor).R,
					0,
				)
			} else {
				r.gotoIndex(-1, -1)
			}
		case ActionSearchAgain, ActionSearchReverse:
			delta := 1
			if msg.Type == ActionSearchReverse {
				delta = -1
			}

			if !r.isSelectionMode {
				r.gotoMatchDelta(delta)
			}
		case ActionSearchForward, ActionSearchBackward:
			r.isSearching = true
			r.isForward = msg.Type == ActionSearchForward
			r.searchInput.Reset()
		case ActionTimeStepBack:
			r.gotoIndex(r.location.Index-1, -1)
		case ActionTimeStepForward:
			r.gotoIndex(r.location.Index+1, -1)
		case ActionScrollUpHalf:
			r.moveCursorDelta(-(viewport.R / 2), 0)
		case ActionScrollDownHalf:
			r.moveCursorDelta((viewport.R / 2), 0)
		case ActionScrollUp:
			r.setScroll(r.offset.R - 1)
		case ActionScrollDown:
			r.setScroll(r.offset.R + 1)
		case ActionCursorDown:
			r.moveCursorDelta(1, 0)
		case ActionCursorUp:
			r.moveCursorDelta(-1, 0)
		case ActionCursorLeft:
			r.moveCursorDelta(0, -1)
		case ActionCursorRight:
			r.moveCursorDelta(0, 1)
		}
	}

	return r, nil
}

func newReplay(
	events []sessions.Event,
	binds *bind.Engine[bind.Action],
) *Replay {
	ti := textinput.New()
	ti.Focus()
	ti.CharLimit = 20
	ti.Width = 20
	ti.Prompt = ""
	m := &Replay{
		render:       taro.NewRenderer(),
		events:       events,
		terminal:     emu.New(),
		searchInput:  ti,
		playbackRate: 1,
		binds:        binds,
	}
	m.gotoIndex(-1, -1)
	return m
}

func New(
	ctx context.Context,
	recorder *sessions.Recorder,
	replayBinds *bind.BindScope,
	replayEvents chan<- bind.BindEvent,
) *taro.Program {
	events := recorder.Events()

	engine := bind.NewEngine[bind.Action]()
	engine.SetScopes(replayBinds)
	go engine.Poll(ctx)
	go func() {
		for {
			select {
			case <-ctx.Done():
				return
			case event := <-engine.Recv():
				if bindEvent, ok := event.(bind.BindEvent); ok {
					replayEvents <- bindEvent
				}
			}
		}
	}()

	return taro.New(ctx, newReplay(events, engine))
}
