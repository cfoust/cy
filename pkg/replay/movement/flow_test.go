package movement

import (
	"testing"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/sessions"

	"github.com/stretchr/testify/require"
)

func createFlowTest(terminal emu.Terminal, size geom.Size) *flowMovement {
	movement := NewFlow(terminal, size)
	i := movement.(*flowMovement)
	return i
}

func TestCenter(t *testing.T) {
	s := sessions.NewSimulator()
	s.Add(
		geom.Size{R: 4, C: 10},
		emu.LineFeedMode,
		"foo\nbar\nbaz",
	)

	before := s.Terminal()

	s.Add("\nsix")
	after := s.Terminal()

	r := createFlowTest(before, geom.Size{R: 3, C: 10})

	// the first write should not force us to move
	require.Equal(t, geom.Vec2{R: 0, C: 0}, r.root)
	require.Equal(t, geom.Vec2{R: 2, C: 3}, r.cursor)

	// the next will, though
	r = createFlowTest(after, geom.Size{R: 3, C: 10})
	// centers the cursor on the screen
	require.Equal(t, geom.Vec2{R: 1, C: 3}, r.cursor)
	require.Equal(t, geom.Vec2{R: 2, C: 0}, r.root)
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
	r.ScrollYDelta(1)
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
	require.Equal(t, geom.Vec2{R: 2, C: 0}, r.root)
	require.Equal(t, geom.Vec2{R: 1, C: 4}, r.cursor)
	require.Equal(t, 4, r.desiredCol)

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

func testHighlight(
	t *testing.T,
	m Movement,
	size geom.Size,
	highlights []Highlight,
	lines ...string,
) {
	bg := emu.Color(1)
	for i := range highlights {
		highlights[i].BG = bg
	}

	state := tty.New(size)
	m.View(state, highlights)
	image := state.Image

	for row := range lines {
		for col, char := range lines[row] {
			switch char {
			case '0':
				require.NotEqual(
					t,
					bg,
					image[row][col].BG,
					"cell [%d, %d] should not be highlighted",
					row,
					col,
				)
			case '1':
				require.Equal(
					t,
					bg,
					image[row][col].BG,
					"cell [%d, %d] should be highlighted",
					row,
					col,
				)
			default:
				t.Logf(
					"invalid char %+v, must be 1 or 0",
					char,
				)
				t.FailNow()
			}
		}
	}
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

	testHighlight(t, r, size,
		[]Highlight{
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

	testHighlight(t, r, size,
		[]Highlight{
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
	testHighlight(t, r, size,
		[]Highlight{
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

	testHighlight(t, r, size,
		[]Highlight{
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

	testHighlight(t, r, size,
		[]Highlight{
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

func TestReadStringFlow(t *testing.T) {
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
}