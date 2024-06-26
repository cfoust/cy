package flow

import (
	"testing"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/replay/movement"
	"github.com/cfoust/cy/pkg/sessions"

	"github.com/stretchr/testify/require"
)

func TestCenter(t *testing.T) {
	s := sessions.NewSimulator()
	s.Add(
		geom.Size{R: 4, C: 4},
		emu.LineFeedMode,
		"foo bar\nbaz",
	)

	before := s.Terminal()

	s.Add("\nsix")
	after := s.Terminal()

	r := createFlowTest(before, geom.Size{R: 3, C: 4})

	// the first write should not force us to move
	require.Equal(t, geom.Vec2{R: 0, C: 0}, r.root)
	require.Equal(t, geom.Vec2{R: 2, C: 3}, r.cursor)

	// the next will, though
	r = createFlowTest(after, geom.Size{R: 3, C: 4})
	require.Equal(t, geom.Vec2{R: 0, C: 4}, r.root)
	require.Equal(t, geom.Vec2{R: 2, C: 3}, r.cursor)
}

func TestPast(t *testing.T) {
	s := sessions.NewSimulator()
	s.Add(
		geom.Size{R: 3, C: 10},
		emu.LineFeedMode,
		"foo\nbar\n",
	)

	r := createFlowTest(s.Terminal(), geom.Size{R: 2, C: 10})
	require.Equal(t, geom.Vec2{R: 1, C: 0}, r.root)
	require.Equal(t, geom.Vec2{R: 1, C: 0}, r.cursor)
}

func TestResize(t *testing.T) {
	s := sessions.NewSimulator()
	s.Add(
		geom.Size{R: 5, C: 10},
		emu.LineFeedMode,
		"foo\nbar\nbaz",
	)

	r := createFlowTest(s.Terminal(), geom.Size{R: 3, C: 10})
	require.Equal(t, geom.Vec2{R: 2, C: 3}, r.cursor)
	require.Equal(t, geom.Vec2{R: 0, C: 0}, r.root)

	// We have not yet moved, so we should keep the OG cursor
	r.Resize(geom.Size{R: 3, C: 3})
	require.Equal(t, geom.Vec2{R: 2, C: 2}, r.cursor)

	r.MoveCursorX(-1)
	require.Equal(t, geom.Vec2{R: 2, C: 1}, r.cursor)

	r.Resize(geom.Size{R: 10, C: 1})
	// The centering should push us down a bit
	require.Equal(t, geom.Vec2{R: 0, C: 2}, r.root)
	// Cursor is on the "a" in "baz"
	require.Equal(t, geom.Vec2{R: 5, C: 0}, r.cursor)
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

	r := createFlowTest(s.Terminal(), geom.Size{R: 2, C: 10})

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
	require.Equal(t, geom.Vec2{R: 5, C: 0}, r.root)
	require.Equal(t, geom.Vec2{R: 1, C: 5}, r.cursor)
	require.Equal(t, 5, r.desiredCol)

	r.ScrollYDelta(-1)
	// five
	// si[x]
	require.Equal(t, geom.Vec2{R: 4, C: 0}, r.root)
	require.Equal(t, geom.Vec2{R: 1, C: 2}, r.cursor)

	r.ScrollYDelta(-1)
	// four
	// fiv[e]
	require.Equal(t, geom.Vec2{R: 3, C: 0}, r.root)
	require.Equal(t, geom.Vec2{R: 1, C: 3}, r.cursor)

	r.ScrollYDelta(1)
	// fiv[e]
	// six
	require.Equal(t, geom.Vec2{R: 4, C: 0}, r.root)
	require.Equal(t, geom.Vec2{R: 0, C: 3}, r.cursor)

	r.ScrollYDelta(1)
	// si[x]
	// seven
	require.Equal(t, geom.Vec2{R: 5, C: 0}, r.root)
	require.Equal(t, geom.Vec2{R: 0, C: 2}, r.cursor)

	r.ScrollTop()
	require.Equal(t, geom.Vec2{R: 0, C: 0}, r.root)
	require.Equal(t, geom.Vec2{R: 0, C: 2}, r.cursor)

	// should not cause a panic
	r.ScrollYDelta(-1)

	r.ScrollBottom()
	require.Equal(t, geom.Vec2{R: 5, C: 0}, r.root)
	require.Equal(t, geom.Vec2{R: 1, C: 4}, r.cursor)

	r.ScrollYDelta(1)
	require.Equal(t, geom.Vec2{R: 6, C: 0}, r.root)
	// Should not scroll further
	r.ScrollYDelta(100)
	require.Equal(t, geom.Vec2{R: 6, C: 0}, r.root)
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

	r := createFlowTest(s.Terminal(), geom.Size{R: 3, C: 10})
	require.Equal(t, geom.Vec2{R: 1, C: 0}, r.root)
	require.Equal(t, geom.Vec2{R: 2, C: 4}, r.cursor)
	require.Equal(t, 4, r.desiredCol)

	// to align with tests after we changed center logic
	r.ScrollYDelta(1)
	r.MoveCursorY(-1)
	require.Equal(t, geom.Vec2{R: 2, C: 0}, r.root)
	require.Equal(t, geom.Vec2{R: 0, C: 4}, r.cursor)

	r.MoveCursorY(1)
	require.Equal(t, geom.Vec2{R: 2, C: 0}, r.root)
	require.Equal(t, geom.Vec2{R: 1, C: 3}, r.cursor)
	r.MoveCursorY(-1)

	r.MoveCursorY(-1)
	require.Equal(t, geom.Vec2{R: 1, C: 0}, r.root)
	require.Equal(t, geom.Vec2{R: 0, C: 5}, r.cursor)

	r.MoveCursorY(-1)
	// now at top
	require.Equal(t, geom.Vec2{R: 0, C: 0}, r.root)
	require.Equal(t, geom.Vec2{R: 0, C: 2}, r.cursor)

	r.MoveCursorX(1) // does nothing
	require.Equal(t, geom.Vec2{R: 0, C: 0}, r.root)
	require.Equal(t, geom.Vec2{R: 0, C: 2}, r.cursor)

	// move all the way back left
	r.MoveCursorX(-1)
	r.MoveCursorX(-1)
	r.MoveCursorX(-1)
	r.MoveCursorX(-1)
	require.Equal(t, geom.Vec2{R: 0, C: 0}, r.cursor)

	r.MoveCursorY(1)
	require.Equal(t, geom.Vec2{R: 0, C: 0}, r.root)
	require.Equal(t, geom.Vec2{R: 1, C: 5}, r.cursor)

	r.MoveCursorY(1)
	require.Equal(t, geom.Vec2{R: 0, C: 0}, r.root)
	require.Equal(t, geom.Vec2{R: 2, C: 0}, r.cursor)

	// at end of screen
	r.MoveCursorY(1)
	require.Equal(t, geom.Vec2{R: 1, C: 0}, r.root)
	require.Equal(t, geom.Vec2{R: 2, C: 0}, r.cursor)

	// moving down past last occupied line should do nothing
	r.MoveCursorY(1)
	require.Equal(t, geom.Vec2{R: 2, C: 0}, r.cursor)

	// force a big movement
	r.MoveCursorY(-5)
	require.Equal(t, geom.Vec2{R: 0, C: 0}, r.root)
	require.Equal(t, geom.Vec2{R: 0, C: 0}, r.cursor)

	// then back
	r.MoveCursorY(5)
	require.Equal(t, geom.Vec2{R: 1, C: 0}, r.root)
	require.Equal(t, geom.Vec2{R: 2, C: 0}, r.cursor)
}

func TestNormalHighlight(t *testing.T) {
	size := geom.Size{R: 4, C: 10}
	s := sessions.NewSimulator()
	s.Add(
		size,
		emu.LineFeedMode,
		"foo\nbar\nbaz\nblah",
	)

	r := createFlowTest(s.Terminal(), size)
	r.ScrollTop()

	movement.TestHighlight(t, r, size,
		[]movement.Highlight{
			{
				From: geom.Vec2{R: 0, C: 1},
				To:   geom.Vec2{R: 2, C: 1},
			},
		},
		// "foo" line is filled from first "o" onwards
		"0110",
		// "bar" line is completely filled in
		"1110",
		// "baz" line is filled to a
		"1100",
		// "blah" line is untouched
		"0000",
	)
}

func TestLongHighlight(t *testing.T) {
	size := geom.Size{R: 4, C: 3}
	s := sessions.NewSimulator()
	s.Add(
		size,
		emu.LineFeedMode,
		"foobarbaz",
	)

	r := createFlowTest(s.Terminal(), size)
	r.ScrollTop()

	movement.TestHighlight(t, r, size,
		[]movement.Highlight{
			{
				From: geom.Vec2{R: 0, C: 1},
				To:   geom.Vec2{R: 0, C: 7},
			},
		},
		// "foo" line is filled from first "o" onwards
		"011",
		// "bar" line is completely filled in
		"111",
		// "baz" line is filled to a
		"110",
	)

	// Ensure inversion does not break things
	movement.TestHighlight(t, r, size,
		[]movement.Highlight{
			{
				From: geom.Vec2{R: 0, C: 7},
				To:   geom.Vec2{R: 0, C: 1},
			},
		},
		// "foo" line is filled from first "o" onwards
		"011",
		// "bar" line is completely filled in
		"111",
		// "baz" line is filled to a
		"110",
	)
}

func TestLongScreenHighlight(t *testing.T) {
	size := geom.Size{R: 4, C: 3}
	s := sessions.NewSimulator()
	s.Add(
		size,
		emu.LineFeedMode,
		"foobarbaz",
	)

	r := createFlowTest(s.Terminal(), size)
	r.ScrollTop()

	movement.TestHighlight(t, r, size,
		[]movement.Highlight{
			{
				Screen: true,
				From:   geom.Vec2{R: 0, C: 1},
				To:     geom.Vec2{R: 2, C: 1},
			},
		},
		// "foo" line is filled from first "o" onwards
		"011",
		// "bar" line is completely filled in
		"111",
		// "baz" line is filled to a
		"110",
	)
}

func TestCJKHighlight(t *testing.T) {
	size := geom.Size{R: 4, C: 3}
	s := sessions.NewSimulator()
	s.Add(
		size,
		emu.LineFeedMode,
		"fo你好bar",
	)

	r := createFlowTest(s.Terminal(), size)
	r.ScrollTop()

	movement.TestHighlight(t, r, size,
		[]movement.Highlight{
			{
				From: geom.Vec2{R: 0, C: 2},
				To:   geom.Vec2{R: 0, C: 6},
			},
		},
		// "fo" is not highlighted
		"000",
		// "你" occupies two cells
		"110",
		// "好b"
		"111",
		// "ar" is not highlighted
		"000",
	)
}

func TestReadString(t *testing.T) {
	size := geom.Size{R: 4, C: 3}
	s := sessions.NewSimulator()
	s.Add(
		size,
		emu.LineFeedMode,
		"foobar\nbaz\n\ntest",
	)

	r := createFlowTest(s.Terminal(), size)
	r.ScrollTop()

	require.Equal(t, "foobar", r.ReadString(
		geom.Vec2{R: 0, C: 0},
		geom.Vec2{R: 0, C: 5},
	))

	require.Equal(t, "\ntest", r.ReadString(
		geom.Vec2{R: 2, C: 0},
		geom.Vec2{R: 3, C: 3},
	))

	require.Equal(t, "baz\n\ntest", r.ReadString(
		geom.Vec2{R: 1, C: 0},
		geom.Vec2{R: 3, C: 3},
	))

	require.Equal(t, "foobar\nbaz\n\ntest", r.ReadString(
		geom.Vec2{R: 0, C: 0},
		geom.Vec2{R: 3, C: 3},
	))

	require.Equal(t, "oobar\nbaz\n\ntest", r.ReadString(
		geom.Vec2{R: 0, C: 1},
		geom.Vec2{R: 3, C: 3},
	))
	require.Equal(t, "test", r.ReadString(
		geom.Vec2{R: 3, C: 0},
		geom.Vec2{R: 3, C: 4},
	))
}

func TestScrollPast(t *testing.T) {
	s := sessions.NewSimulator()
	s.Add(
		geom.Size{R: 5, C: 10},
		emu.LineFeedMode,
		"one",
	)

	r := createFlowTest(s.Terminal(), geom.Size{R: 2, C: 10})
	r.ScrollYDelta(1)
	require.Equal(t, geom.Vec2{R: 0, C: 0}, r.root)
}

func TestScrollTo(t *testing.T) {
	size := geom.Size{R: 2, C: 10}
	s := sessions.NewSimulator()
	s.Add(
		size,
		emu.LineFeedMode,
		"foo\n",
		"bar\n",
		"baz",
	)

	r := createFlowTest(s.Terminal(), size)

	r.scrollToLine(geom.Vec2{R: 0, C: 0}, ScrollPositionCenter)
	require.Equal(t, geom.Vec2{R: 0, C: 0}, r.root)

	r.scrollToLine(geom.Vec2{R: 0, C: 0}, ScrollPositionTop)
	require.Equal(t, geom.Vec2{R: 0, C: 0}, r.root)

	r.scrollToLine(geom.Vec2{R: 1, C: 0}, ScrollPositionCenter)
	require.Equal(t, geom.Vec2{R: 0, C: 0}, r.root)

	r.scrollToLine(geom.Vec2{R: 2, C: 0}, ScrollPositionCenter)
	require.Equal(t, geom.Vec2{R: 1, C: 0}, r.root)

	r.scrollToLine(geom.Vec2{R: 2, C: 0}, ScrollPositionTop)
	require.Equal(t, geom.Vec2{R: 2, C: 0}, r.root)
}

func TestGetRoot(t *testing.T) {
	size := geom.Size{R: 2, C: 10}
	s := sessions.NewSimulator()
	s.Add(
		size,
		emu.LineFeedMode,
		"foo\n",
		"bar\n",
		"baz\n",
		"\n",
		"foo\n",
		"bar\n",
		"baz\n",
	)

	r := createFlowTest(s.Terminal(), size)

	// Before beginning
	root, ok := r.getRoot(geom.Vec2{R: -1, C: 0})
	require.False(t, ok)

	// After end
	root, ok = r.getRoot(geom.Vec2{R: 6, C: 3})
	require.False(t, ok)

	root, ok = r.getRoot(geom.Vec2{C: 1})
	require.True(t, ok)
	require.Equal(t, geom.Vec2{R: 0, C: 0}, root)

	root, ok = r.getRoot(geom.Vec2{R: 2, C: 2})
	require.True(t, ok)
	require.Equal(t, geom.Vec2{R: 2, C: 0}, root)

	// Should break early
	root, ok = r.getRoot(geom.Vec2{C: 3})
	require.False(t, ok)
}

func TestGoto(t *testing.T) {
	size := geom.Size{R: 2, C: 10}
	s := sessions.NewSimulator()
	s.Add(
		size,
		emu.LineFeedMode,
		"foo\n",
		"bar\n",
		"baz\n",
		"\n", // 3
		"foo\n",
		"bar\n",
		"baz\n", // 6
	)

	r := createFlowTest(s.Terminal(), size)

	r.Goto(geom.Vec2{R: 0, C: 2}) // fo[o]
	require.Equal(t, geom.Vec2{R: 0, C: 0}, r.root)
	require.Equal(t, geom.Vec2{R: 0, C: 2}, r.cursor)

	r.Goto(geom.Vec2{R: 6, C: 0}) // [b]az
	require.Equal(t, geom.Vec2{R: 5, C: 0}, r.root)
	require.Equal(t, geom.Vec2{R: 1, C: 0}, r.cursor)
}

func TestBlank(t *testing.T) {
	size := geom.Size{R: 10, C: 10}
	s := sessions.NewSimulator()
	s.Add(
		size,
		emu.LineFeedMode,
		"foo\n",
		"\n",
		"bar\n",
	)

	r := createFlowTest(s.Terminal(), size)
	r.Snap()
	require.Equal(t, geom.Vec2{R: 3, C: 0}, r.cursor)
	r.MoveCursorY(-2)
	require.Equal(t, geom.Vec2{R: 1, C: 0}, r.cursor)
}
