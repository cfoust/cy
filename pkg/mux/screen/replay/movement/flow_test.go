package movement

import (
	"testing"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/sessions"

	"github.com/stretchr/testify/require"
)

func createFlowTest(terminal emu.Terminal) *flowMovement {
	movement := NewFlow(terminal)
	i := movement.(*flowMovement)
	return i
}

func TestFlow(t *testing.T) {
	s := sessions.NewSimulator()
	s.Add(
		geom.Size{R: 4, C: 10},
		emu.LineFeedMode,
		"foo\nbar\nbaz",
	)

	before := s.Terminal()

	s.Add("\nsix")
	after := s.Terminal()

	r := createFlowTest(before)
	r.Resize(geom.Size{R: 2, C: 10}) // 2x10

	// the first write should not force us to move
	require.Equal(t, geom.Vec2{R: 0, C: 0}, r.root)
	require.Equal(t, geom.Vec2{R: 2, C: 3}, r.cursor)

	// the next will, though
	r = createFlowTest(after)
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

	r := createFlowTest(s.Terminal())
	r.Resize(geom.Size{R: 2, C: 10}) // 2x10

	// -- history:
	// one
	// two
	// -- screen:
	// three
	// four
	// five
	// six
	// seven[ ]

	// seven[ ]
	//
	require.Equal(t, geom.Vec2{
		R: 0,
		C: 5,
	}, r.cursor)
	require.Equal(t, geom.Vec2{
		R: 6,
	}, r.root)
	require.Equal(t, 5, r.desiredCol)

	r.ScrollYDelta(-1)
	// six
	// seven[ ]
	require.Equal(t, geom.Vec2{
		R: 1,
		C: 5,
	}, r.cursor)
	require.Equal(t, geom.Vec2{
		R: 4,
	}, r.root)

	r.ScrollYDelta(-1)
	// four
	// fiv[e]
	require.Equal(t, geom.Vec2{
		R: 1,
		C: 3,
	}, r.cursor)
	require.Equal(t, geom.Vec2{
		R: 3,
	}, r.root)

	r.ScrollYDelta(1)
	// fiv[e]
	// six
	require.Equal(t, geom.Vec2{
		R: 0,
		C: 3,
	}, r.cursor)
	require.Equal(t, geom.Vec2{
		R: 4,
	}, r.root)

	r.ScrollYDelta(1)
	// fiv[e]
	// six
	require.Equal(t, geom.Vec2{
		R: 0,
		C: 3,
	}, r.cursor)
	require.Equal(t, geom.Vec2{
		R: 4,
	}, r.root)
	// si[x]
	// seven
	require.Equal(t, geom.Vec2{
		R: 0,
		C: 2,
	}, r.cursor)
	require.Equal(t, geom.Vec2{
		R: 5,
	}, r.root)

	//i(ActionBeginning)
	//require.Equal(t, -2, r.viewportToTerm(r.cursor).R)

	//i(ActionEnd)
	//require.Equal(t, 4, r.viewportToTerm(r.cursor).R)
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

	r := createFlowTest(s.Terminal())
	r.Resize(geom.Size{R: 3, C: 10}) // 2x10
	require.Equal(t, geom.Vec2{
		R: 1,
		C: 4,
	}, r.cursor)
	require.Equal(t, 4, r.desiredCol)

	r.MoveCursorY(-1)
	require.Equal(t, geom.Vec2{
		R: 0,
		C: 4,
	}, r.cursor)

	r.MoveCursorY(-1)
	require.Equal(t, geom.Vec2{
		R: 0,
		C: 5,
	}, r.cursor)

	r.MoveCursorY(-1)
	require.Equal(t, geom.Vec2{
		R: 0,
		C: 2,
	}, r.cursor)

	r.MoveCursorX(1)
	require.Equal(t, geom.Vec2{
		R: 0,
		C: 2,
	}, r.cursor)

	r.MoveCursorX(-1)
	r.MoveCursorX(-1)
	r.MoveCursorX(-1)
	r.MoveCursorX(-1)
	require.Equal(t, geom.Vec2{
		R: 0,
		C: 0,
	}, r.cursor)

	r.MoveCursorY(1)
	require.Equal(t, geom.Vec2{
		R: 1,
		C: 5,
	}, r.cursor)

	r.MoveCursorY(1)
	require.Equal(t, geom.Vec2{
		R: 1,
		C: 0,
	}, r.cursor)

	// at end of screen
	r.MoveCursorY(1)
	require.Equal(t, geom.Vec2{
		R: 1,
		C: 0,
	}, r.cursor)
	require.Equal(t, geom.Vec2{
		R: 2,
		C: 0,
	}, r.root)

	// moving down past last occupied line should do nothing
	r.MoveCursorY(1)
	require.Equal(t, geom.Vec2{
		R: 1,
		C: 0,
	}, r.cursor)

	// force a big movement
	r.MoveCursorY(-5)
	require.Equal(t, geom.Vec2{
		R: 0,
		C: 0,
	}, r.cursor)
	require.Equal(t, geom.Vec2{
		R: 0,
		C: 0,
	}, r.root)

	// then back
	r.MoveCursorY(5)
	require.Equal(t, geom.Vec2{
		R: 1,
		C: 0,
	}, r.cursor)
	require.Equal(t, geom.Vec2{
		R: 2,
		C: 0,
	}, r.root)
}
