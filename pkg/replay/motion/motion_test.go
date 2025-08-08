package motion

import (
	"testing"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/re"

	"github.com/stretchr/testify/require"
)

func TestJump(t *testing.T) {
	m := fromLines("The five boxing wizards jump quickly. a")
	m.Goto(geom.Vec2{C: 38})

	Jump(m, "T", false, false)
	require.Equal(t, geom.Vec2{}, m.Cursor())
	Jump(m, "T", false, false)
	require.Equal(t, geom.Vec2{}, m.Cursor())
	Jump(m, "a", true, false)
	require.Equal(t, geom.Vec2{C: 19}, m.Cursor())
	Jump(m, "a", true, false)
	require.Equal(t, geom.Vec2{C: 38}, m.Cursor())
	Jump(m, "T", false, false)
	require.Equal(t, geom.Vec2{}, m.Cursor())
	Jump(m, "x", true, true)
	require.Equal(t, geom.Vec2{C: 10}, m.Cursor())
	Jump(m, "e", false, true)
	require.Equal(t, geom.Vec2{C: 8}, m.Cursor())
}

func TestStartOfLine(t *testing.T) {
	m := fromLines("foobarbaz")
	m.Goto(geom.Vec2{C: 5})
	StartOfLine(m)
	require.Equal(t, geom.Vec2{}, m.Cursor())
}

func TestFirstNonBlank(t *testing.T) {
	m := fromLines("  foo")
	m.Goto(geom.Vec2{C: 4})
	FirstNonBlank(m)
	require.Equal(t, geom.Vec2{C: 2}, m.Cursor())
}

func TestEndOfLine(t *testing.T) {
	{
		m := fromLines("foo")
		EndOfLine(m)
		require.Equal(t, geom.Vec2{C: 2}, m.Cursor())
	}

	// this is valid in image mode
	{
		m := fromLines("foo ")
		EndOfLine(m)
		require.Equal(t, geom.Vec2{C: 3}, m.Cursor())
	}

	{
		m := fromLines("你好吗")
		EndOfLine(m)
		require.Equal(t, geom.Vec2{C: 4}, m.Cursor())
	}
}

func TestLastNonBlank(t *testing.T) {
	{
		m := fromLines("foo   ")
		LastNonBlank(m)
		require.Equal(t, geom.Vec2{C: 2}, m.Cursor())
	}

	{
		m := fromLines("你好吗   ")
		LastNonBlank(m)
		require.Equal(t, geom.Vec2{C: 4}, m.Cursor())
	}
}

func TestStartOfScreenLine(t *testing.T) {
	m := fromLines(
		"foo",
		"bar",
	)

	m.cursor = geom.Vec2{
		R: 1,
		C: 2,
	}
	m.lines[1].R = 0
	m.lines[1].C0 = 3
	m.lines[1].C1 = 6

	StartOfScreenLine(m)
	require.Equal(t, geom.Vec2{
		R: 0,
		C: 3,
	}, m.Cursor())
}

func TestMiddleOfScreenLine(t *testing.T) {
	m := fromLines("foobar")
	m.size.C = 10
	MiddleOfScreenLine(m)
	require.Equal(t, geom.Vec2{C: 5}, m.Cursor())
}

func TestMiddleOfLine(t *testing.T) {
	m := fromLines("foobar")
	MiddleOfLine(m)
	require.Equal(t, geom.Vec2{C: 3}, m.Cursor())
}

func TestEndOfScreenLine(t *testing.T) {
	m := fromLines("foobar")
	EndOfScreenLine(m)
	require.Equal(t, geom.Vec2{C: 5}, m.Cursor())
}

func TestFirstNonBlankScreen(t *testing.T) {
	m := fromLines("  foo")
	m.Goto(geom.Vec2{C: 4})
	FirstNonBlankScreen(m)
	require.Equal(t, geom.Vec2{C: 2}, m.Cursor())
}

func TestLastNonBlankScreen(t *testing.T) {
	{
		m := fromLines("foo   ")
		LastNonBlankScreen(m)
		require.Equal(t, geom.Vec2{C: 2}, m.Cursor())
	}

	{
		m := fromLines("你好吗   ")
		LastNonBlankScreen(m)
		require.Equal(t, geom.Vec2{C: 4}, m.Cursor())
	}
}

func TestFindNext(t *testing.T) {
	m := fromLines(
		"foofoofoo",
		"barbarbar",
		"bazbazbaz",
	)

	// basic forwards
	{
		to, ok := findNext(
			m,
			re.MakePattern("foo"),
			geom.Vec2{},
			true,
		)
		require.True(t, ok)
		require.Equal(t,
			emu.ScreenLine{
				R:     0,
				C0:    3,
				C1:    6,
				Chars: emu.LineFromString("foo"),
			},
			to,
		)
	}

	// basic backwards
	{
		to, ok := findNext(
			m,
			re.MakePattern("foo"),
			geom.Vec2{R: 0, C: 3},
			false,
		)
		require.True(t, ok)
		require.Equal(t,
			emu.ScreenLine{
				R:     0,
				C0:    0,
				C1:    3,
				Chars: emu.LineFromString("foo"),
			},
			to,
		)
	}

	// edge case: going backwards should still let pattern pass origin
	{
		to, ok := findNext(
			m,
			re.MakePattern("foo"),
			geom.Vec2{R: 0, C: 2},
			false,
		)
		require.True(t, ok)
		require.Equal(t,
			emu.ScreenLine{
				R:     0,
				C0:    0,
				C1:    3,
				Chars: emu.LineFromString("foo"),
			},
			to,
		)
	}

	// find on the next line
	{
		to, ok := findNext(
			m,
			re.MakePattern("bar"),
			geom.Vec2{},
			true,
		)
		require.True(t, ok)
		require.Equal(t,
			emu.ScreenLine{
				R:     1,
				C0:    0,
				C1:    3,
				Chars: emu.LineFromString("bar"),
			},
			to,
		)
	}

	// from.C = -1 should allow matching beginning of line
	{
		to, ok := findNext(
			m,
			re.MakePattern("foo"),
			geom.Vec2{C: -1},
			true,
		)
		require.True(t, ok)
		require.Equal(t,
			emu.ScreenLine{
				R:     0,
				C0:    0,
				C1:    3,
				Chars: emu.LineFromString("foo"),
			},
			to,
		)
	}

	// from.C = len(line) should allow matching end of line
	{
		to, ok := findNext(
			m,
			re.MakePattern("baz"),
			geom.Vec2{R: 2, C: 9},
			false,
		)
		require.True(t, ok)
		require.Equal(t,
			emu.ScreenLine{
				R:     2,
				C0:    6,
				C1:    9,
				Chars: emu.LineFromString("baz"),
			},
			to,
		)
	}

	// Empty
	{
		_, ok := findNext(
			m,
			re.MakePattern(""),
			geom.Vec2{R: 2, C: 9},
			false,
		)
		require.False(t, ok)
	}

	// Missing
	{
		_, ok := findNext(
			m,
			re.MakePattern("asdasdasd"),
			geom.Vec2{R: 0, C: 0},
			false,
		)
		require.False(t, ok)
	}
}
