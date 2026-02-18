package replay

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/keys"
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

func createTest(
	events []sessions.Event,
	options ...Option,
) (*Replay, func(msgs ...interface{})) {
	var r = newReplay(
		context.Background(),
		player.FromEvents(events),
		bind.NewEngine[bind.Action](),
		bind.NewEngine[bind.Action](),
		options...,
	)

	// If we don't do this, the cursor blink causes tests to hang forever
	r.input.Cursor.SetMode(cursor.CursorHide)

	var m taro.Model = r

	test := taro.Test(m, taro.WithKittyKeys)

	return r, func(msgs ...interface{}) {
		var realMsg tea.Msg
		for _, msg := range msgs {
			realMsg = msg
			switch msg := msg.(type) {
			case ActionType:
				realMsg = ActionEvent{Type: msg}
			case int:
				realMsg = forceTimeEvent{index: msg}
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
	t.Logf("r.location: %+v", r.Location())
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
	i(2, ActionSearchReverse)
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
	_, i := createTest([]sessions.Event{})
	// should not panic
	i(
		geom.Size{R: 3, C: 10},
	)
	i(forceTimeDeltaEvent{
		delta:          0,
		skipInactivity: true,
	})
	i(ActionSearchAgain)
}

func TestMouseClickAndDrag(t *testing.T) {
	events := sim().
		Add(
			geom.Size{R: 5, C: 10},
			emu.LineFeedMode,
			"hello world\nthis is a test\nfoobar baz",
		).
		Events()

	r, i := createTest(events)

	// Set up the replay in copy mode
	i(geom.Size{R: 5, C: 10})
	i(ActionBeginning)
	r.enterCopyMode()

	clickMsg := taro.MouseMsg{
		Vec2:   geom.Vec2{R: 1, C: 2},
		Type:   keys.MousePress,
		Button: keys.MouseLeft,
		Down:   true,
	}
	i(clickMsg)

	require.True(t, r.isSelecting)

	dragMsg := taro.MouseMsg{
		Vec2:   geom.Vec2{R: 1, C: 5},
		Type:   keys.MouseMotion,
		Button: keys.MouseLeft,
		Down:   true,
	}
	i(dragMsg)

	require.True(t, r.isSelecting)

	releaseMsg := taro.MouseMsg{
		Vec2:   geom.Vec2{R: 1, C: 5},
		Type:   keys.MousePress,
		Button: keys.MouseLeft,
		Down:   false,
	}
	i(releaseMsg)

	require.True(t, r.isSelecting)
}

func TestMouseClickOutsideCopyMode(t *testing.T) {
	events := sim().
		Add(
			geom.Size{R: 5, C: 10},
			emu.LineFeedMode,
			"hello world\ntest",
		).
		Events()

	r, i := createTest(events)

	i(geom.Size{R: 5, C: 10})
	i(ActionBeginning)
	require.False(t, r.isCopyMode())

	clickMsg := taro.MouseMsg{
		Vec2:   geom.Vec2{R: 1, C: 2},
		Type:   keys.MousePress,
		Button: keys.MouseLeft,
		Down:   true,
	}
	i(clickMsg)

	require.True(t, r.isSelecting)
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
	i(size, 0)
	require.Equal(t, e[0].Stamp, r.currentTime)

	// Move into first text event
	i(forceTimeDeltaEvent{
		delta:          delta,
		skipInactivity: true,
	})
	require.Equal(t, 1, r.Location().Index)

	// Play again, which should trigger a skip since the time is greater
	// than IDLE_THRESHOLD
	i(forceTimeDeltaEvent{
		delta:          delta,
		skipInactivity: true,
	})
	require.Equal(t, 2, r.Location().Index)
	require.Equal(t, e[2].Stamp, r.currentTime)

	// Go backwards, which should bring us back to the previous event
	i(forceTimeDeltaEvent{
		delta:          -delta,
		skipInactivity: true,
	})
	require.Equal(t, 1, r.Location().Index)
	require.Equal(t, e[2].Stamp.Add(-delta), r.currentTime)

	// Backwards again, which will skip inactivity back to 0
	i(forceTimeDeltaEvent{
		delta:          -delta,
		skipInactivity: true,
	})
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
	i(size, 0)

	// Simulate a jump forward
	i(forceTimeDeltaEvent{
		delta:          5 * time.Minute,
		skipInactivity: false,
	})
	require.Equal(t, 2, r.Location().Index)
	require.Equal(t, r.currentTime.Sub(e[0].Stamp), 5*time.Minute)
	// And then a play
	i(forceTimeDeltaEvent{
		delta:          time.Second,
		skipInactivity: r.skipInactivity,
	})
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
	i(geom.DEFAULT_SIZE, 0)
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
		r, _ := createTest(
			s.Events(),
			WithCopyMode,
		)
		require.Equal(t, ModeCopy, r.mode)
	}

	// WithFlow
	{
		r, _ := createTest(
			s.Events(),
			WithFlow,
		)
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

		r, i := createTest(
			s.Events(),
			WithLocation(geom.Vec2{R: 1, C: 2}), // ba[r]
		)
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
			emu.OSC133Prompt, "command"+emu.OSC133CommandExec+"\n",
			"foo\n"+emu.OSC133CommandDone,
			emu.OSC133Prompt, "command"+emu.OSC133CommandExec+"\n",
			"foo\n"+emu.OSC133CommandDone,
			emu.OSC133Prompt,
		)

	r, i := createTest(s.Events())
	i(geom.DEFAULT_SIZE, 0)

	// Time mode
	i(ActionCommandForward)
	require.Equal(t, 3, r.Location().Index)
	i(ActionCommandForward)
	require.Equal(t, 6, r.Location().Index)
	i(ActionCommandBackward)
	require.Equal(t, 3, r.Location().Index)
	i(8)

	r.enterCopyMode()

	// Copy mode
	i(ActionCommandBackward)
	require.Equal(t, geom.Vec2{R: 2, C: 2}, r.movement.Cursor())
	i(ActionCommandBackward)
	require.Equal(t, geom.Vec2{R: 0, C: 2}, r.movement.Cursor())
	i(ActionCommandForward)
	require.Equal(t, geom.Vec2{R: 2, C: 2}, r.movement.Cursor())

	// Selection
	i(ActionCommandSelectBackward)
	require.True(t, r.isSelecting)
	require.Equal(t, geom.Vec2{R: 1, C: 0}, r.movement.Cursor())
	require.Equal(t, geom.Vec2{R: 1, C: 3}, r.selectStart)

	i(ActionCommandSelectForward)
	require.True(t, r.isSelecting)
	require.Equal(t, geom.Vec2{R: 3, C: 0}, r.movement.Cursor())
	require.Equal(t, geom.Vec2{R: 3, C: 3}, r.selectStart)
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
	r.enterCopyMode()

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

func TestPlayback(t *testing.T) {
	size := geom.Size{R: 5, C: 10}
	e := sim().
		Add(size).
		AddTime(0, "test").
		AddTime(IDLE_THRESHOLD*2, "test").
		AddTime(time.Second, "test").
		Events()

	r, i := createTest(e)
	i(size, 0, ActionTimePlay)
	require.Equal(t, 3, r.Location().Index)
	require.False(t, r.isPlaying)

	r.playbackRate = -1
	i(ActionTimePlay)
	require.Equal(t, 0, r.Location().Index)
	require.False(t, r.isPlaying)
}

func TestScrollCursorPosition(t *testing.T) {
	s := sessions.NewSimulator().
		Add(
			geom.Size{R: 26, C: 80},
			emu.LineFeedMode,
		)
	for j := 0; j < 100; j++ {
		s.Add(fmt.Sprintf("line %02d\n", j))
	}

	size := geom.Size{R: 26, C: 80}
	usableRows := 25 // -1 for status bar

	r, send := createTest(s.Events())
	send(size)
	r.enterCopyMode()
	send(ActionBeginning)

	// Move cursor far enough that all three commands can freely
	// reposition the viewport
	for j := 0; j < 40; j++ {
		send(ActionCursorDown)
	}

	cursor := r.movement.Cursor()

	// zt: cursor to top of viewport
	send(ActionScrollCursorTop)
	_, _, vc := r.movement.Viewport()
	require.Equal(t, 0, vc.R)
	require.Equal(t, cursor, r.movement.Cursor())

	// zb: cursor to bottom of viewport
	send(ActionScrollCursorBottom)
	_, _, vc = r.movement.Viewport()
	require.Equal(t, usableRows-1, vc.R)
	require.Equal(t, cursor, r.movement.Cursor())

	// zz: cursor to center of viewport
	send(ActionScrollCursorCenter)
	_, _, vc = r.movement.Viewport()
	require.Equal(t, usableRows/2, vc.R)
	require.Equal(t, cursor, r.movement.Cursor())
}
