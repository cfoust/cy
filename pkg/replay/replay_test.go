package replay

import (
	"testing"
	"time"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/replay/detect"
	"github.com/cfoust/cy/pkg/replay/player"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/charmbracelet/bubbles/cursor"
	tea "github.com/charmbracelet/bubbletea"

	"github.com/stretchr/testify/require"
	"github.com/xo/terminfo"
)

var sim = sessions.NewSimulator

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
	r.incrInput.Cursor.SetMode(cursor.CursorHide)

	var m taro.Model = r

	test := taro.Test(m)

	return r, func(msgs ...interface{}) {
		var realMsg tea.Msg
		for _, msg := range msgs {
			realMsg = msg
			switch msg := msg.(type) {
			case ActionType:
				realMsg = ActionEvent{Type: msg}
			}

			test(realMsg)
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
	r.forceTimeDelta(delta, true)
	require.Equal(t, 1, r.Location().Index)

	// Play again, which should trigger a skip since the time is greater
	// than IDLE_THRESHOLD
	r.forceTimeDelta(delta, true)
	require.Equal(t, 2, r.Location().Index)
	require.Equal(t, e[2].Stamp, r.currentTime)

	// Go backwards, which should bring us back to the previous event
	r.forceTimeDelta(-delta, true)
	require.Equal(t, 1, r.Location().Index)
	require.Equal(t, e[2].Stamp.Add(-delta), r.currentTime)

	// Backwards again, which will skip inactivity back to 0
	r.forceTimeDelta(-delta, true)
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
	r.forceTimeDelta(5*time.Minute, false)
	require.Equal(t, 2, r.Location().Index)
	require.Equal(t, r.currentTime.Sub(e[0].Stamp), 5*time.Minute)
	// And then a play
	r.forceTimeDelta(time.Second, r.skipInactivity)
	require.Greater(t, r.currentTime.Sub(e[0].Stamp), 5*time.Minute)
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

func TestSwap(t *testing.T) {
	s := sessions.NewSimulator().
		Add(
			geom.Size{R: 10, C: 10},
			"foo\nbar\nbaz",
			emu.EnterAltScreen,
		)

	r, i := createTest(s.Events())
	require.True(t, r.IsAltMode())
	require.False(t, r.isSwapped)

	i(ActionSwapScreen)
	require.Equal(t, r.mode, ModeCopy)
	require.True(t, r.isSwapped)

	i(ActionSwapScreen)
	require.False(t, r.isSwapped)

	i(ActionSwapScreen, ActionQuit)
	require.Equal(t, r.mode, ModeTime)
	require.False(t, r.isSwapped)
}

func TestOptions(t *testing.T) {
	s := sessions.NewSimulator().
		Add(
			geom.Size{R: 10, C: 10},
			"foo\nbar\nbaz",
			emu.EnterAltScreen,
		)

	// WithCopyMode
	{
		r, _ := createTest(s.Events())
		WithCopyMode(r)
		require.Equal(t, ModeCopy, r.mode)
	}

	// WithFlow
	{
		r, _ := createTest(s.Events())
		WithFlow(r)
		require.Equal(t, ModeCopy, r.mode)
		require.True(t, r.isFlowMode())
	}

	// WithLocation
	{
		// No alt mode
		s := sessions.NewSimulator().
			Add(
				geom.Size{R: 10, C: 10},
				"foo\nbar\nbaz",
			)

		r, i := createTest(s.Events())
		WithLocation(geom.Vec2{R: 1, C: 2})(r) // ba[r]
		i(geom.DEFAULT_SIZE)
		require.Equal(t, ModeCopy, r.mode)
		require.Equal(t, geom.Vec2{R: 1, C: 2}, r.movement.Cursor())
	}
}

func TestJumpCommand(t *testing.T) {
	s := sessions.NewSimulator().
		Add(
			emu.LineFeedMode,
			geom.Size{R: 10, C: 10},
			detect.TEST_PROMPT, "command\n",
			"foo\n",
			detect.TEST_PROMPT, "command\n",
			"foo\n",
			detect.TEST_PROMPT,
		)

	r, i := createTest(s.Events())
	i(geom.DEFAULT_SIZE)
	r.forceIndex(0, -1)

	// Time mode
	i(ActionCommandForward)
	require.Equal(t, 3, r.Location().Index)
	i(ActionCommandForward)
	require.Equal(t, 6, r.Location().Index)
	i(ActionCommandBackward)
	require.Equal(t, 3, r.Location().Index)
	r.forceIndex(8, -1)

	WithCopyMode(r)

	// Copy mode
	i(ActionCommandBackward)
	require.Equal(t, geom.Vec2{R: 2, C: 2}, r.movement.Cursor())
	i(ActionCommandBackward)
	require.Equal(t, geom.Vec2{R: 0, C: 2}, r.movement.Cursor())
	i(ActionCommandForward)
	require.Equal(t, geom.Vec2{R: 2, C: 2}, r.movement.Cursor())
}

func TestIncremental(t *testing.T) {
	s := sessions.NewSimulator().
		Add(
			geom.Size{R: 10, C: 10},
			emu.LineFeedMode,
			"foo\nbar\nbaz",
		)

	r, i := createTest(s.Events())
	i(geom.DEFAULT_SIZE)
	WithCopyMode(r)

	// Go forwards
	i(ActionSearchForward, "ba")
	require.True(t, r.incr.IsActive())
	require.Equal(t, geom.Vec2{R: 1, C: 0}, r.movement.Cursor())

	// Accept it
	i("enter")
	require.Equal(t, geom.Vec2{R: 1, C: 0}, r.movement.Cursor())
	require.False(t, r.incr.IsActive())

	// Next
	i(ActionSearchAgain)
	require.Equal(t, geom.Vec2{R: 2, C: 0}, r.movement.Cursor())

	// Go backwards
	i(ActionSearchBackward, "foo")
	require.True(t, r.incr.IsActive())
	require.Equal(t, geom.Vec2{R: 0, C: 0}, r.movement.Cursor())

	// Next
	i(ActionSearchAgain)
	require.Equal(t, geom.Vec2{R: 0, C: 0}, r.movement.Cursor())
}
