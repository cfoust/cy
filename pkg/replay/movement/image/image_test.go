package image

import (
	"testing"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/replay/movement"
	"github.com/cfoust/cy/pkg/sessions"

	"github.com/stretchr/testify/require"
	"github.com/xo/terminfo"
)

var sim = sessions.NewSimulator

func createImageTest(terminal emu.Terminal, size geom.Size) *imageMovement {
	movement := New(terminal, size)
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

func TestReadStringImage(t *testing.T) {
	s := sessions.NewSimulator().
		Add(
			geom.Size{R: 5, C: 10},
			emu.LineFeedMode,
			"foo\n",
			"你好\n",
			"bar\n",
			"\n",
			"baz",
		)

	r := createImageTest(s.Terminal(), geom.Size{R: 5, C: 10})
	require.Equal(t, `foo`, r.ReadString(
		geom.Vec2{R: 0, C: 0},
		geom.Vec2{R: 0, C: 2},
		movement.SelectChar,
	))

	require.Equal(t, "fo\n你", r.ReadString(
		geom.Vec2{R: 0, C: 0},
		geom.Vec2{R: 1, C: 1},
		movement.SelectChar,
	))

	require.Equal(t, "foo\n你好", r.ReadString(
		geom.Vec2{R: 0, C: 0},
		geom.Vec2{R: 1, C: 2},
		movement.SelectChar,
	))

	require.Equal(t, "bar\n\nbaz", r.ReadString(
		geom.Vec2{R: 2, C: 0},
		geom.Vec2{R: 4, C: 2},
		movement.SelectChar,
	))
}

func TestReadStringLine(t *testing.T) {
	s := sessions.NewSimulator().
		Add(
			geom.Size{R: 5, C: 10},
			emu.LineFeedMode,
			"foo\n",
			"你好\n",
			"bar\n",
			"\n",
			"baz",
		)

	r := createImageTest(s.Terminal(), geom.Size{R: 5, C: 10})

	// SelectLine expands to full line width regardless of column
	require.Equal(t, "foo", r.ReadString(
		geom.Vec2{R: 0, C: 1},
		geom.Vec2{R: 0, C: 1},
		movement.SelectLine,
	))

	// Multi-line
	require.Equal(t, "foo\n你好", r.ReadString(
		geom.Vec2{R: 0, C: 2},
		geom.Vec2{R: 1, C: 0},
		movement.SelectLine,
	))

	// Includes empty line
	require.Equal(t, "bar\n\nbaz", r.ReadString(
		geom.Vec2{R: 2, C: 1},
		geom.Vec2{R: 4, C: 0},
		movement.SelectLine,
	))

	// Inverted range
	require.Equal(t, "foo\n你好", r.ReadString(
		geom.Vec2{R: 1, C: 0},
		geom.Vec2{R: 0, C: 2},
		movement.SelectLine,
	))
}

func TestReadStringBlock(t *testing.T) {
	s := sessions.NewSimulator().
		Add(
			geom.Size{R: 4, C: 10},
			emu.LineFeedMode,
			"abcde\n",
			"fghij\n",
			"klmno\n",
			"pqrst",
		)

	r := createImageTest(s.Terminal(), geom.Size{R: 4, C: 10})

	// Block: rectangular region cols 1-3
	require.Equal(t, "bcd\nghi\nlmn\nqrs", r.ReadString(
		geom.Vec2{R: 0, C: 1},
		geom.Vec2{R: 3, C: 3},
		movement.SelectChar,
	))

	// Single column
	require.Equal(t, "a\nf\nk\np", r.ReadString(
		geom.Vec2{R: 0, C: 0},
		geom.Vec2{R: 3, C: 0},
		movement.SelectChar,
	))

	// Inverted columns (image normalizes via normalizeBoxRange)
	require.Equal(t, "bcd\nghi\nlmn\nqrs", r.ReadString(
		geom.Vec2{R: 0, C: 3},
		geom.Vec2{R: 3, C: 1},
		movement.SelectChar,
	))
}

func TestImageLineHighlight(t *testing.T) {
	s := sessions.NewSimulator()
	s.Add(
		geom.Size{R: 5, C: 5},
		emu.LineFeedMode,
		"xxxxx\nxxxxx\nxxxxx\nxxxxx\nxxxxx",
	)

	size := geom.Size{R: 3, C: 3}
	i := createImageTest(s.Terminal(), size)
	i.ScrollTop()
	i.ScrollXDelta(-10)

	// SelectLine highlights full rows regardless of column
	movement.TestHighlight(t, i, size,
		[]movement.Highlight{
			{
				Selection: movement.SelectLine,
				From:      geom.Vec2{R: 1, C: 2},
				To:        geom.Vec2{R: 2, C: 1},
			},
		},
		"000",
		"111",
		"111",
	)

	// Single line
	movement.TestHighlight(t, i, size,
		[]movement.Highlight{
			{
				Selection: movement.SelectLine,
				From:      geom.Vec2{R: 0, C: 3},
				To:        geom.Vec2{R: 0, C: 3},
			},
		},
		"111",
		"000",
		"000",
	)

	// Inverted range
	movement.TestHighlight(t, i, size,
		[]movement.Highlight{
			{
				Selection: movement.SelectLine,
				From:      geom.Vec2{R: 2, C: 0},
				To:        geom.Vec2{R: 1, C: 0},
			},
		},
		"000",
		"111",
		"111",
	)

	// Line highlight with viewport scrolled horizontally
	i.ScrollXDelta(2)
	movement.TestHighlight(t, i, size,
		[]movement.Highlight{
			{
				Selection: movement.SelectLine,
				From:      geom.Vec2{R: 0, C: 0},
				To:        geom.Vec2{R: 0, C: 0},
			},
		},
		"111",
		"000",
		"000",
	)
}

func TestImageBlockHighlight(t *testing.T) {
	s := sessions.NewSimulator()
	s.Add(
		geom.Size{R: 5, C: 5},
		emu.LineFeedMode,
		"xxxxx\nxxxxx\nxxxxx\nxxxxx\nxxxxx",
	)

	size := geom.Size{R: 3, C: 3}
	i := createImageTest(s.Terminal(), size)
	i.ScrollTop()
	i.ScrollXDelta(-10)

	// Image mode already treats non-Screen highlights as boxes,
	// so SelectBlock should behave the same as default
	movement.TestHighlight(t, i, size,
		[]movement.Highlight{
			{
				Selection: movement.SelectBlock,
				From:      geom.Vec2{R: 0, C: 1},
				To:        geom.Vec2{R: 2, C: 2},
			},
		},
		"011",
		"011",
		"011",
	)

	// Inverted columns
	movement.TestHighlight(t, i, size,
		[]movement.Highlight{
			{
				Selection: movement.SelectBlock,
				From:      geom.Vec2{R: 0, C: 2},
				To:        geom.Vec2{R: 2, C: 1},
			},
		},
		"011",
		"011",
		"011",
	)
}

func TestImageCircleHighlight(t *testing.T) {
	s := sessions.NewSimulator()
	s.Add(
		geom.Size{R: 5, C: 5},
		emu.LineFeedMode,
		"xxxxx\nxxxxx\nxxxxx\nxxxxx\nxxxxx",
	)

	size := geom.Size{R: 5, C: 5}
	i := createImageTest(s.Terminal(), size)
	i.ScrollTop()
	i.ScrollXDelta(-10)

	movement.TestHighlight(t, i, size,
		[]movement.Highlight{
			{
				Selection: movement.SelectCircle,
				From:      geom.Vec2{R: 0, C: 0},
				To:        geom.Vec2{R: 4, C: 4},
			},
		},
		"01110",
		"11111",
		"11111",
		"11111",
		"01110",
	)

	// 3x3 circle
	size3 := geom.Size{R: 3, C: 3}
	i3 := createImageTest(s.Terminal(), size3)
	i3.ScrollTop()
	i3.ScrollXDelta(-10)

	movement.TestHighlight(t, i3, size3,
		[]movement.Highlight{
			{
				Selection: movement.SelectCircle,
				From:      geom.Vec2{R: 0, C: 0},
				To:        geom.Vec2{R: 2, C: 2},
			},
		},
		"010",
		"111",
		"010",
	)
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

	movement.TestHighlight(t, i, size,
		[]movement.Highlight{
			{
				From: geom.Vec2{R: 0, C: 1},
				To:   geom.Vec2{R: 4, C: 2},
			},
		},
		"011",
		"011",
		"011",
	)

	// Invert
	movement.TestHighlight(t, i, size,
		[]movement.Highlight{
			{
				From: geom.Vec2{R: 4, C: 2},
				To:   geom.Vec2{R: 0, C: 1},
			},
		},
		"011",
		"011",
		"011",
	)

	// Non-screen highlights can also be inverted the other way
	movement.TestHighlight(t, i, size,
		[]movement.Highlight{
			{
				From: geom.Vec2{R: 0, C: 2},
				To:   geom.Vec2{R: 4, C: 1},
			},
		},
		"011",
		"011",
		"011",
	)

	i.ScrollXDelta(2)
	movement.TestHighlight(t, i, size,
		[]movement.Highlight{
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
	movement.TestHighlight(t, i, size,
		[]movement.Highlight{
			{
				From: geom.Vec2{R: 0, C: 1},
				To:   geom.Vec2{R: 0, C: 2},
			},
		},
		"011",
		"000",
		"000",
	)

	movement.TestHighlight(t, i, size,
		[]movement.Highlight{
			{
				From: geom.Vec2{R: 0, C: 1},
				To:   geom.Vec2{R: 1, C: 2},
			},
		},
		"011",
		"011",
		"000",
	)

	movement.TestHighlight(t, i, size,
		[]movement.Highlight{
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

	movement.TestHighlight(t, i, size,
		[]movement.Highlight{
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

	movement.TestHighlight(t, i, size,
		[]movement.Highlight{
			{
				From: geom.Vec2{R: 3, C: 0},
				To:   geom.Vec2{R: 3, C: 4},
			},
		},
		"000",
		"000",
		"000",
	)

	movement.TestHighlight(t, i, size,
		[]movement.Highlight{
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

func TestViewportToMovement(t *testing.T) {
	s := sim().
		Add(
			geom.Size{R: 5, C: 5},
			emu.LineFeedMode,
			"abcde\nfghij\nklmno\npqrst\nuvwxy",
		).
		Term(terminfo.CursorAddress, 2, 2)

	size := geom.Size{R: 3, C: 3}
	i := createImageTest(s.Terminal(), size)

	// Test basic coordinate translation with no offset
	i.offset = geom.Vec2{R: 0, C: 0}
	result := i.ViewportToMovement(geom.Vec2{R: 1, C: 1})
	require.Equal(t, geom.Vec2{R: 1, C: 1}, result)

	// Test coordinate translation with offset
	i.offset = geom.Vec2{R: 1, C: 1}
	result = i.ViewportToMovement(geom.Vec2{R: 0, C: 0})
	require.Equal(t, geom.Vec2{R: 1, C: 1}, result)

	result = i.ViewportToMovement(geom.Vec2{R: 2, C: 2})
	require.Equal(t, geom.Vec2{R: 3, C: 3}, result)

	// Test edge cases
	result = i.ViewportToMovement(geom.Vec2{R: -1, C: -1})
	require.Equal(t, geom.Vec2{R: 0, C: 0}, result)
}
