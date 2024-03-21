package replay

import (
	"testing"
	"time"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux/screen/replay/player"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/charmbracelet/bubbles/cursor"
	tea "github.com/charmbracelet/bubbletea"

	"github.com/stretchr/testify/require"
	"github.com/xo/terminfo"
)

var sim = sessions.NewSimulator

func arg(_type ActionType, arg string) ActionEvent {
	return ActionEvent{
		Type: _type,
		Arg:  arg,
	}
}

func createTestSession() []sessions.Event {
	return sim().
		Add(
			emu.LineFeedMode,
			geom.DEFAULT_SIZE,
			"test string please ignore",
		).
		Term(terminfo.ClearScreen).
		Add("take two").
		Term(terminfo.ClearScreen).
		Add("test").
		Events()
}

func createTest(events []sessions.Event) (*Replay, func(msgs ...interface{})) {
	var r = newReplay(
		player.FromEvents(events),
		bind.NewEngine[bind.Action](),
	)

	// If we don't do this, the cursor blink causes tests to hang forever
	r.searchInput.Cursor.SetMode(cursor.CursorHide)

	var m taro.Model = r

	return r, func(msgs ...interface{}) {
		var cmd, subCmd tea.Cmd
		var realMsg tea.Msg

		var handleCmd func(tea.Cmd)
		handleCmd = func(cmd tea.Cmd) {
			for cmd != nil {
				msg := cmd()

				switch msg := msg.(type) {
				case cursor.BlinkMsg:
				case tea.BatchMsg:
					for _, cmd := range msg {
						m, subCmd = m.Update(cmd())
						handleCmd(subCmd)
					}
					cmd = nil
				default:
					m, cmd = m.Update(msg)
				}
				m.View(tty.New(geom.DEFAULT_SIZE))
			}
		}

		for _, msg := range msgs {
			realMsg = msg
			switch msg := msg.(type) {
			case ActionType:
				realMsg = ActionEvent{Type: msg}
			case geom.Size:
				realMsg = tea.WindowSizeMsg{
					Width:  msg.C,
					Height: msg.R,
				}
			case string:
				keyMsgs := taro.KeysToMsg(msg)
				if len(keyMsgs) == 1 {
					realMsg = keyMsgs[0]
				}
			}

			m, cmd = m.Update(realMsg)
			handleCmd(cmd)
			m.View(tty.New(geom.DEFAULT_SIZE))
		}
	}
}

func TestSearch(t *testing.T) {
	s := sessions.NewSimulator().
		Add(
			// TODO(cfoust): 10/27/23 why does R=2 lead to fewer matches?
			geom.Size{R: 10, C: 10},
			"foo",
			"bar blah", // 2
			"foo",
			"foo",
			"foo",
			"foo",
			"bar", // 7
			"foo",
			"bar", // 9
			"foo",
		)

	r, i := createTest(s.Events())
	r.searchInput.Cursor.Blink = false
	r.searchProgress = nil
	i(
		ActionBeginning,
		ActionSearchForward,
		"bar",
		"enter",
	)
	require.Equal(t, 3, len(r.matches))
	require.Equal(t, 2, r.Location().Index)
	i(ActionSearchAgain)
	require.Equal(t, 7, r.Location().Index)
	i(ActionSearchReverse)
	require.Equal(t, 2, r.Location().Index)
	// Loop back from beginning
	i(ActionSearchReverse)
	require.Equal(t, 9, r.Location().Index)
	// Loop over end
	i(ActionSearchAgain)
	require.Equal(t, 2, r.Location().Index)

	// Ensure that the -1 rewriting works
	r.forceIndex(2, -1)
	i(ActionSearchReverse)
	require.Equal(t, 2, r.Location().Index)

	// Now search backward
	i(ActionEnd, ActionSearchBackward, "bar", "enter")
	require.Equal(t, 9, r.Location().Index)
	i(ActionSearchAgain)
	require.Equal(t, 7, r.Location().Index)
	i(ActionSearchReverse)
	require.Equal(t, 9, r.Location().Index)
}

func TestIndex(t *testing.T) {
	r, _ := createTest(createTestSession())
	r.forceIndex(2, 0)
	require.Equal(t, "t ", r.getLine(0).String()[:2])
	r.forceIndex(2, 1)
	require.Equal(t, "te ", r.getLine(0).String()[:3])
	r.forceIndex(2, 0)
	require.Equal(t, "t ", r.getLine(0).String()[:2])
	r.forceIndex(2, -1)
	require.Equal(t, "test", r.getLine(0).String()[:4])
	r.forceIndex(4, -1)
	require.Equal(t, "take", r.getLine(0).String()[:4])
}

func TestViewport(t *testing.T) {
	s := sim().
		Add(geom.Size{R: 20, C: 20}).
		Term(terminfo.ClearScreen).
		Term(terminfo.CursorAddress, 19, 19)

	r, i := createTest(s.Events())
	i(geom.Size{R: 10, C: 10})
	require.Equal(t, geom.Vec2{R: 0, C: 0}, r.minOffset)
	require.Equal(t, geom.Vec2{R: 11, C: 10}, r.maxOffset)
	require.Equal(t, geom.Vec2{R: 11, C: 10}, r.offset)
}

func TestFlow(t *testing.T) {
	s := sessions.NewSimulator()
	s.Add(
		geom.Size{R: 4, C: 10},
		emu.LineFeedMode,
		"foo\nbar\nbaz",
		"\nsix",
	)

	r, i := createTest(s.Events())
	i(geom.Size{R: 4, C: 10})

	// the first write should not force us to move
	r.forceIndex(2, -1)
	require.Equal(t, geom.Vec2{R: 0, C: 0}, r.root)
	require.Equal(t, geom.Vec2{R: 2, C: 3}, r.cursor)

	// the next will, though
	r.forceIndex(3, -1)
	// centers the cursor on the screen
	require.Equal(t, geom.Vec2{R: 1, C: 3}, r.cursor)
	require.Equal(t, geom.Vec2{R: 2, C: 0}, r.root)
}

func TestScroll(t *testing.T) {
	s := sessions.NewSimulator()
	s.Add(
		geom.Size{R: 5, C: 10},
		emu.LineFeedMode,
		"one\n",
		"two\n",
		"three\n",
		"four\n",
		"five\n",
		"six\n",
		"seven",
	)

	r, i := createTest(s.Events())
	i(geom.Size{R: 3, C: 10}) // 2x10
	r.forceIndex(8, -1)

	// -- history:
	// one
	// two
	// -- screen:
	// three
	// four
	// five
	// six
	// seven[ ]

	// six
	// seven[ ]
	require.Equal(t, geom.Vec2{
		R: 1,
		C: 5,
	}, r.cursor)
	require.Equal(t, geom.Vec2{
		R: 5,
	}, r.root)
	require.Equal(t, 5, r.desiredCol)

	i(ActionScrollUp)
	// five
	// si[x]
	require.Equal(t, geom.Vec2{
		R: 1,
		C: 2,
	}, r.cursor)
	require.Equal(t, geom.Vec2{
		R: 4,
	}, r.root)

	i(ActionScrollUp)
	// four
	// fiv[e]
	require.Equal(t, geom.Vec2{
		R: 1,
		C: 3,
	}, r.cursor)
	require.Equal(t, geom.Vec2{
		R: 3,
	}, r.root)

	i(ActionScrollDown)
	// fiv[e]
	// six
	require.Equal(t, geom.Vec2{
		R: 0,
		C: 3,
	}, r.cursor)
	require.Equal(t, geom.Vec2{
		R: 4,
	}, r.root)

	i(ActionScrollDown)
	// si[x]
	// seven
	require.Equal(t, geom.Vec2{
		R: 0,
		C: 2,
	}, r.cursor)
	require.Equal(t, geom.Vec2{
		R: 5,
	}, r.root)

	i(ActionBeginning)
	require.Equal(t, -2, r.viewportToTerm(r.cursor).R)

	i(ActionEnd)
	require.Equal(t, 4, r.viewportToTerm(r.cursor).R)
}

func TestCursor(t *testing.T) {
	s := sessions.NewSimulator()
	s.Add(
		geom.Size{R: 5, C: 10},
		emu.LineFeedMode,
		"foo\n",
		"      foo\n",
		"foo  foo\n",
		"foo ",
	)

	r, i := createTest(s.Events())
	i(geom.Size{R: 3, C: 10})
	r.forceIndex(5, -1)
	require.Equal(t, geom.Vec2{
		R: 1,
		C: 4,
	}, r.cursor)
	require.Equal(t, 4, r.desiredCol)

	i(ActionCursorUp)
	require.Equal(t, geom.Vec2{
		R: 0,
		C: 4,
	}, r.cursor)

	i(ActionCursorUp)
	require.Equal(t, geom.Vec2{
		R: 0,
		C: 5,
	}, r.cursor)

	i(ActionCursorUp)
	require.Equal(t, geom.Vec2{
		R: 0,
		C: 2,
	}, r.cursor)

	i(ActionCursorRight)
	require.Equal(t, geom.Vec2{
		R: 0,
		C: 2,
	}, r.cursor)

	i(ActionCursorLeft, ActionCursorLeft, ActionCursorLeft, ActionCursorLeft)
	require.Equal(t, geom.Vec2{
		R: 0,
		C: 0,
	}, r.cursor)

	i(ActionCursorDown)
	require.Equal(t, geom.Vec2{
		R: 1,
		C: 5,
	}, r.cursor)

	i(ActionCursorDown)
	require.Equal(t, geom.Vec2{
		R: 1,
		C: 0,
	}, r.cursor)

	// at end of screen
	i(ActionCursorDown)
	require.Equal(t, geom.Vec2{
		R: 1,
		C: 0,
	}, r.cursor)
	require.Equal(t, geom.Vec2{
		R: 2,
		C: 0,
	}, r.root)

	// moving down past last occupied line should do nothing
	i(ActionCursorDown)
	require.Equal(t, geom.Vec2{
		R: 1,
		C: 0,
	}, r.cursor)
}

func TestEmpty(t *testing.T) {
	s := sim().Add(geom.Size{R: 5, C: 10})
	_, i := createTest(s.Events())
	i(geom.Size{R: 3, C: 10}, ActionCursorDown)
	// should not panic
}

func TestTime(t *testing.T) {
	delta := time.Second / PLAYBACK_FPS
	size := geom.Size{R: 5, C: 10}
	e := sim().
		Add(size).
		AddTime(0, "test").
		AddTime(IDLE_THRESHOLD*2, "test").
		AddTime(time.Second, "test").
		Events()

	r, i := createTest(e)
	i(size)
	r.forceIndex(0, -1)
	require.Equal(t, e[0].Stamp, r.currentTime)

	// Move into first text event
	r.setTimeDelta(delta, true)
	require.Equal(t, 1, r.Location().Index)

	// Play again, which should trigger a skip since the time is greater
	// than IDLE_THRESHOLD
	r.setTimeDelta(delta, true)
	require.Equal(t, 2, r.Location().Index)
	require.Equal(t, e[2].Stamp, r.currentTime)

	// Go backwards, which should bring us back to the previous event
	r.setTimeDelta(-delta, true)
	require.Equal(t, 1, r.Location().Index)
	require.Equal(t, e[2].Stamp.Add(-delta), r.currentTime)

	// Backwards again, which will skip inactivity back to 0
	r.setTimeDelta(-delta, true)
	require.Equal(t, 0, r.Location().Index)
	require.Equal(t, e[0].Stamp, r.currentTime)
}

func TestTimeBug(t *testing.T) {
	size := geom.Size{R: 5, C: 10}
	e := sim().
		Add(size).
		AddTime(0, "test").
		AddTime(3*time.Minute, "test").
		AddTime(60*time.Minute, "test").
		Events()

	r, i := createTest(e)
	i(size)
	r.forceIndex(0, -1)

	// Simulate a jump forward
	r.setTimeDelta(5*time.Minute, false)
	require.Equal(t, 2, r.Location().Index)
	require.Equal(t, r.currentTime.Sub(e[0].Stamp), 5*time.Minute)
	// And then a play
	r.setTimeDelta(time.Second, r.skipInactivity)
	require.Greater(t, r.currentTime.Sub(e[0].Stamp), 5*time.Minute)
}

func TestReadString(t *testing.T) {
	s := sessions.NewSimulator().
		Add(
			geom.Size{R: 5, C: 10},
			emu.LineFeedMode,
			"foo\n",
			"你好 ",
		)

	r, i := createTest(s.Events())
	i(geom.Size{R: 6, C: 10})
	require.Equal(t, `foo`, r.readString(
		geom.Vec2{R: 0, C: 0},
		geom.Vec2{R: 0, C: 2},
	))
	require.Equal(t, `foo`, r.readString(
		geom.Vec2{R: 0, C: 2},
		geom.Vec2{R: 0, C: 0},
	))
	require.Equal(t, "foo\n你", r.readString(
		geom.Vec2{R: 0, C: 0},
		geom.Vec2{R: 1, C: 0},
	))
	require.Equal(t, "foo\n你", r.readString(
		geom.Vec2{R: 1, C: 0},
		geom.Vec2{R: 0, C: 0},
	))
	require.Equal(t, "oo", r.readString(
		geom.Vec2{R: 0, C: 1},
		geom.Vec2{R: 0, C: 2},
	))
	require.Equal(t, "o", r.readString(
		geom.Vec2{R: 0, C: 2},
		geom.Vec2{R: 0, C: 2},
	))
	require.Equal(t, "你好", r.readString(
		geom.Vec2{R: 1, C: 0},
		geom.Vec2{R: 1, C: 3},
	))
}

func TestTimeJump(t *testing.T) {
	size := geom.Size{R: 5, C: 10}
	e := sim().
		Add(size).
		AddTime(0, "test").
		AddTime(3*time.Minute, "test").
		AddTime(60*time.Minute, "test").
		Events()

	r, i := createTest(e)
	i(size, ActionBeginning, ActionSearchForward, "5m", "enter")
	require.Equal(t, r.currentTime.Sub(e[0].Stamp), 5*time.Minute)
	i(ActionSearchBackward, "5m", "enter")
	require.Equal(t, 0, r.Location().Index)
}

func TestPrompt(t *testing.T) {
	r, i := createTest(createTestSession())
	i(geom.DEFAULT_SIZE)
	r.forceIndex(0, -1)
	i(ActionSearchForward, "blah", ActionQuit)
	require.Equal(t, r.mode, ModeTime)
}

func TestJump(t *testing.T) {
	s := sessions.NewSimulator()
	s.Add(
		geom.Size{R: 5, C: 50},
		emu.LineFeedMode,
		"The five boxing wizards jump quickly. a",
	)

	r, i := createTest(s.Events())
	i(geom.Size{R: 3, C: 50})
	i(arg(ActionJumpBackward, "T"))
	require.Equal(t, geom.Vec2{}, r.cursor)
	i(ActionCursorRight, ActionCursorRight, ActionJumpAgain)
	require.Equal(t, geom.Vec2{}, r.cursor)
	i(arg(ActionJumpForward, "a"))
	require.Equal(t, geom.Vec2{C: 19}, r.cursor)
	i(ActionJumpAgain)
	require.Equal(t, geom.Vec2{C: 38}, r.cursor)
	i(arg(ActionJumpBackward, "T"))
	require.Equal(t, geom.Vec2{}, r.cursor)
	i(arg(ActionJumpToForward, "x"))
	require.Equal(t, geom.Vec2{C: 10}, r.cursor)
	i(arg(ActionJumpToBackward, "e"))
	require.Equal(t, geom.Vec2{C: 8}, r.cursor)
}
