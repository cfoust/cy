package movement

import (
	"testing"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/sessions"

	"github.com/stretchr/testify/require"
	"github.com/xo/terminfo"
)

var sim = sessions.NewSimulator

func createImageTest(terminal emu.Terminal, size geom.Size) *imageMovement {
	movement := NewImage(terminal, size)
	i := movement.(*imageMovement)
	return i
}

func TestViewport(t *testing.T) {
	s := sim().
		Add(geom.Size{R: 20, C: 20}).
		Term(terminfo.ClearScreen).
		Term(terminfo.CursorAddress, 19, 19)

	i := createImageTest(s.Terminal(), geom.Size{R: 9, C: 10})
	require.Equal(t, geom.Vec2{R: 0, C: 0}, i.minOffset)
	require.Equal(t, geom.Vec2{R: 11, C: 10}, i.maxOffset)
	require.Equal(t, geom.Vec2{R: 11, C: 10}, i.offset)
}

func TestReadString(t *testing.T) {
	s := sessions.NewSimulator().
		Add(
			geom.Size{R: 5, C: 10},
			emu.LineFeedMode,
			"foo\n",
			"你好 ",
		)

	r := createImageTest(s.Terminal(), geom.Size{R: 5, C: 10})
	require.Equal(t, `foo`, r.ReadString(
		geom.Vec2{R: 0, C: 0},
		geom.Vec2{R: 0, C: 2},
	))
	require.Equal(t, `foo`, r.ReadString(
		geom.Vec2{R: 0, C: 2},
		geom.Vec2{R: 0, C: 0},
	))
	require.Equal(t, "foo\n你", r.ReadString(
		geom.Vec2{R: 0, C: 0},
		geom.Vec2{R: 1, C: 0},
	))
	require.Equal(t, "foo\n你", r.ReadString(
		geom.Vec2{R: 1, C: 0},
		geom.Vec2{R: 0, C: 0},
	))
	require.Equal(t, "oo", r.ReadString(
		geom.Vec2{R: 0, C: 1},
		geom.Vec2{R: 0, C: 2},
	))
	require.Equal(t, "o", r.ReadString(
		geom.Vec2{R: 0, C: 2},
		geom.Vec2{R: 0, C: 2},
	))
	require.Equal(t, "你好", r.ReadString(
		geom.Vec2{R: 1, C: 0},
		geom.Vec2{R: 1, C: 3},
	))
}

func TestJump(t *testing.T) {
	s := sessions.NewSimulator()
	size := geom.Size{R: 5, C: 50}
	s.Add(
		size,
		emu.LineFeedMode,
		"The five boxing wizards jump quickly. a",
	)

	i := createImageTest(s.Terminal(), size)
	f := createFlowTest(s.Terminal(), size)
	for _, m := range []Movement{f, i} {
		m.Jump("T", false, false)
		require.Equal(t, geom.Vec2{}, m.Cursor())
		m.MoveCursorX(1)
		m.MoveCursorX(1)
		m.Jump("T", false, false)
		require.Equal(t, geom.Vec2{}, m.Cursor())
		m.Jump("a", true, false)
		require.Equal(t, geom.Vec2{C: 19}, m.Cursor())
		m.Jump("a", true, false)
		require.Equal(t, geom.Vec2{C: 38}, m.Cursor())
		m.Jump("T", false, false)
		require.Equal(t, geom.Vec2{}, m.Cursor())
		m.Jump("x", true, true)
		require.Equal(t, geom.Vec2{C: 10}, m.Cursor())
		m.Jump("e", false, true)
		require.Equal(t, geom.Vec2{C: 8}, m.Cursor())
	}
}

func TestInteractions(t *testing.T) {
	s := sim().
		Add(
			geom.Size{R: 5, C: 5},
			emu.LineFeedMode,
			"f  oo\nb  ar\nb  az",
		).
		Term(terminfo.CursorAddress, 0, 0)

	size := geom.Size{R: 3, C: 3}
	i := createImageTest(s.Terminal(), size)
	require.Equal(t, geom.Vec2{}, i.Cursor())

	///////////////////////////
	// Cursor-directed movement
	///////////////////////////

	// X
	////

	// Move to edge of viewport
	i.MoveCursorX(2)
	require.Equal(t, geom.Vec2{R: 0, C: 2}, i.cursor)
	require.Equal(t, geom.Vec2{R: 0, C: 0}, i.offset)

	// should shift viewport right
	i.MoveCursorX(1)
	require.Equal(t, geom.Vec2{R: 0, C: 2}, i.cursor)
	require.Equal(t, geom.Vec2{R: 0, C: 1}, i.offset)
	i.MoveCursorX(1)
	require.Equal(t, geom.Vec2{R: 0, C: 2}, i.cursor)
	require.Equal(t, geom.Vec2{R: 0, C: 2}, i.offset)

	// Go back to origin
	i.MoveCursorX(-5)
	require.Equal(t, geom.Vec2{R: 0, C: 0}, i.cursor)
	require.Equal(t, geom.Vec2{R: 0, C: 0}, i.offset)

	// Y
	////

	// Move to edge of viewport
	i.MoveCursorY(2)
	require.Equal(t, geom.Vec2{R: 2, C: 0}, i.cursor)
	require.Equal(t, geom.Vec2{R: 0, C: 0}, i.offset)

	// should shift viewport down
	i.MoveCursorY(1)
	require.Equal(t, geom.Vec2{R: 2, C: 0}, i.cursor)
	require.Equal(t, geom.Vec2{R: 1, C: 0}, i.offset)
	i.MoveCursorY(1)
	require.Equal(t, geom.Vec2{R: 2, C: 0}, i.cursor)
	require.Equal(t, geom.Vec2{R: 2, C: 0}, i.offset)

	// Go back to origin
	i.MoveCursorY(-5)
	require.Equal(t, geom.Vec2{R: 0, C: 0}, i.cursor)
	require.Equal(t, geom.Vec2{R: 0, C: 0}, i.offset)

	///////////////////////////
	// Scroll-directed movement
	///////////////////////////

	// X
	////

	i.ScrollXDelta(1)
	require.Equal(t, geom.Vec2{R: 0, C: 0}, i.cursor)
	require.Equal(t, geom.Vec2{R: 0, C: 1}, i.offset)

	i.ScrollXDelta(1)
	require.Equal(t, geom.Vec2{R: 0, C: 0}, i.cursor)
	require.Equal(t, geom.Vec2{R: 0, C: 2}, i.offset)

	i.ScrollXDelta(-2)
	require.Equal(t, geom.Vec2{R: 0, C: 2}, i.cursor)
	require.Equal(t, geom.Vec2{R: 0, C: 0}, i.offset)

	i.MoveCursorX(-2)

	// Y
	////

	i.ScrollYDelta(1)
	require.Equal(t, geom.Vec2{R: 0, C: 0}, i.cursor)
	require.Equal(t, geom.Vec2{R: 1, C: 0}, i.offset)

	i.ScrollYDelta(1)
	require.Equal(t, geom.Vec2{R: 0, C: 0}, i.cursor)
	require.Equal(t, geom.Vec2{R: 2, C: 0}, i.offset)

	i.ScrollYDelta(-2)
	require.Equal(t, geom.Vec2{R: 2, C: 0}, i.cursor)
	require.Equal(t, geom.Vec2{R: 0, C: 0}, i.offset)
}

func TestImageHighlight(t *testing.T) {
	s := sessions.NewSimulator()
	s.Add(
		geom.Size{R: 5, C: 5},
		emu.LineFeedMode,
		// fill the screen
		"xxxxx\nxxxxx\nxxxxx\nxxxxx\nxxxxx",
	)

	size := geom.Size{R: 3, C: 3}
	i := createImageTest(s.Terminal(), size)
	i.ScrollTop()
	i.ScrollXDelta(-10)
	require.Equal(t, geom.Vec2{R: 0, C: 0}, i.offset)

	testHighlight(t, i, size,
		[]Highlight{
			{
				From: geom.Vec2{R: 0, C: 1},
				To:   geom.Vec2{R: 4, C: 2},
			},
		},
		"011",
		"011",
		"011",
	)

	i.ScrollXDelta(2)
	testHighlight(t, i, size,
		[]Highlight{
			{
				From: geom.Vec2{R: 0, C: 1},
				To:   geom.Vec2{R: 4, C: 2},
			},
		},
		"100",
		"100",
		"100",
	)

	i.ScrollXDelta(-2)
	testHighlight(t, i, size,
		[]Highlight{
			{
				From: geom.Vec2{R: 0, C: 1},
				To:   geom.Vec2{R: 0, C: 2},
			},
		},
		"011",
		"000",
		"000",
	)

	testHighlight(t, i, size,
		[]Highlight{
			{
				From: geom.Vec2{R: 0, C: 1},
				To:   geom.Vec2{R: 1, C: 2},
			},
		},
		"011",
		"011",
		"000",
	)

	testHighlight(t, i, size,
		[]Highlight{
			{
				Screen: true,
				From:   geom.Vec2{R: 0, C: 1},
				To:     geom.Vec2{R: 2, C: 1},
			},
		},
		"011",
		"111",
		"110",
	)

	testHighlight(t, i, size,
		[]Highlight{
			{
				Screen: true,
				From:   geom.Vec2{R: 0, C: 1},
				To:     geom.Vec2{R: 0, C: 2},
			},
		},
		"011",
		"000",
		"000",
	)

	testHighlight(t, i, size,
		[]Highlight{
			{
				From: geom.Vec2{R: 3, C: 0},
				To:   geom.Vec2{R: 3, C: 4},
			},
		},
		"000",
		"000",
		"000",
	)

	testHighlight(t, i, size,
		[]Highlight{
			{
				Screen: true,
				From:   geom.Vec2{R: 3, C: 0},
				To:     geom.Vec2{R: 3, C: 4},
			},
		},
		"000",
		"000",
		"000",
	)
}
