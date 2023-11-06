package replay

import (
	"testing"
	"time"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/stretchr/testify/require"
	"github.com/xo/terminfo"
)

var sim = sessions.NewSimulator

func createTestSession() []sessions.Event {
	return sim().
		Add(
			"\033[20h", // CRLF -- why is this everywhere?
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
	var r = newReplay(events, bind.NewEngine[bind.Action](), make(chan interface{}, 1000))
	var m taro.Model = r

	return r, func(msgs ...interface{}) {
		var cmd tea.Cmd
		var realMsg tea.Msg
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
			m.View(tty.New(geom.DEFAULT_SIZE))
			for cmd != nil {
				m, cmd = m.Update(cmd())
				m.View(tty.New(geom.DEFAULT_SIZE))
			}
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
	i(ActionBeginning, ActionSearchForward, "bar", "enter")
	require.Equal(t, 3, len(r.matches))
	require.Equal(t, 2, r.location.Index)
	i(ActionSearchAgain)
	require.Equal(t, 7, r.location.Index)
	i(ActionSearchReverse)
	require.Equal(t, 2, r.location.Index)
	// Loop back from beginning
	i(ActionSearchReverse)
	require.Equal(t, 9, r.location.Index)
	// Loop over end
	i(ActionSearchAgain)
	require.Equal(t, 2, r.location.Index)

	// Ensure that the -1 rewriting works
	r.gotoIndex(2, -1)
	i(ActionSearchReverse)
	require.Equal(t, 2, r.location.Index)

	// Now search backward
	i(ActionEnd, ActionSearchBackward, "bar", "enter")
	require.Equal(t, 9, r.location.Index)
	i(ActionSearchAgain)
	require.Equal(t, 7, r.location.Index)
	i(ActionSearchReverse)
	require.Equal(t, 9, r.location.Index)
}

func TestIndex(t *testing.T) {
	r, _ := createTest(createTestSession())
	r.gotoIndex(2, 0)
	require.Equal(t, "t ", r.getLine(0).String()[:2])
	r.gotoIndex(2, 1)
	require.Equal(t, "te ", r.getLine(0).String()[:3])
	r.gotoIndex(2, 0)
	require.Equal(t, "t ", r.getLine(0).String()[:2])
	r.gotoIndex(2, -1)
	require.Equal(t, "test", r.getLine(0).String()[:4])
	r.gotoIndex(4, -1)
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

func TestScroll(t *testing.T) {
	s := sessions.NewSimulator()
	s.Add(
		geom.Size{R: 5, C: 10},
		"\033[20h", // CRLF -- why is this everywhere?
		"one\n",
		"two\n",
		"three\n",
		"four\n",
		"five\n",
		"six\n",
		"seven",
	)

	r, i := createTest(s.Events())
	i(geom.Size{R: 3, C: 10})
	require.Equal(t, 1, r.cursor.R)
	require.Equal(t, 5, r.cursor.C)
	require.Equal(t, 5, r.desiredCol)
	// six
	// seven[ ]

	i(ActionScrollUp)
	// five
	// si[x]
	require.Equal(t, 2, r.cursor.C)
	require.Equal(t, 5, r.desiredCol)

	i(ActionScrollUp)
	// four
	// fiv[e]
	require.Equal(t, 3, r.cursor.C)
	require.Equal(t, 5, r.desiredCol)

	i(ActionScrollDown)
	// fiv[e]
	// six
	require.Equal(t, 0, r.cursor.R)
	require.Equal(t, 3, r.cursor.C)

	i(ActionScrollDown)
	// si[x]
	// seven
	require.Equal(t, 0, r.cursor.R)
	require.Equal(t, 2, r.cursor.C)

	i(ActionBeginning)
	require.Equal(t, -2, r.viewportToTerm(r.cursor).R)

	i(ActionEnd)
	require.Equal(t, 4, r.viewportToTerm(r.cursor).R)
}

func TestCursor(t *testing.T) {
	s := sessions.NewSimulator()
	s.Add(
		geom.Size{R: 5, C: 10},
		"\033[20h", // CRLF -- why is this everywhere?
		"foo\n",
		"      foo\n",
		"foo  foo\n",
		"foo ",
	)

	r, i := createTest(s.Events())
	i(geom.Size{R: 3, C: 10})
	require.Equal(t, 2, r.offset.R)
	require.Equal(t, 1, r.cursor.R)
	require.Equal(t, 4, r.cursor.C)
	require.Equal(t, 4, r.desiredCol)
	i(ActionCursorUp)
	require.Equal(t, 4, r.cursor.C)
	i(ActionCursorUp)
	require.Equal(t, 5, r.cursor.C)
	i(ActionCursorUp)
	require.Equal(t, 2, r.cursor.C)
	i(ActionCursorRight)
	require.Equal(t, 2, r.cursor.C)
	i(ActionCursorLeft, ActionCursorLeft, ActionCursorLeft, ActionCursorLeft)
	require.Equal(t, 0, r.cursor.C)
	i(ActionCursorDown)
	require.Equal(t, 5, r.cursor.C)
	i(ActionCursorDown)
	require.Equal(t, 0, r.cursor.C)

	// at end of screen
	i(ActionCursorDown)
	require.Equal(t, 0, r.cursor.C)
	require.Equal(t, 1, r.cursor.R)

	// moving down past last occupied line should do nothing
	i(ActionCursorDown)
	require.Equal(t, geom.Vec2{
		R: 3,
		C: 0,
	}, r.viewportToTerm(r.cursor))
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
	r.gotoIndex(0, -1)
	require.Equal(t, e[0].Stamp, r.currentTime)
	r.setTimeDelta(delta, true)
	require.Equal(t, 1, r.location.Index)
	r.setTimeDelta(delta, true)
	require.Equal(t, 2, r.location.Index)
	require.Equal(t, e[2].Stamp, r.currentTime)
	r.setTimeDelta(-delta, true)
	require.Equal(t, 1, r.location.Index)
	r.setTimeDelta(-delta, true)
	require.Equal(t, 0, r.location.Index)
}

func TestReadString(t *testing.T) {
	s := sessions.NewSimulator().
		Add(
			geom.Size{R: 5, C: 10},
			"\033[20h", // CRLF -- why is this everywhere?
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
	hour := 3600 * time.Second
	size := geom.Size{R: 5, C: 10}
	e := sim().
		Add(size).
		AddTime(0, "test").
		AddTime(hour, "test").
		Events()

	r, i := createTest(e)
	i(size)
	r.gotoIndex(1, -1)
	i(ActionSearchForward, "+5m", "enter")
	require.Equal(t, e[0].Stamp.Add(5*time.Minute), r.currentTime)
	i(ActionSearchForward, "-5m", "enter")
	require.Equal(t, e[0].Stamp, r.currentTime)
}
