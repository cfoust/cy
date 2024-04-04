package emu

import (
	"testing"

	"github.com/cfoust/cy/pkg/geom"

	"github.com/stretchr/testify/require"
)

func TestFlowLines(t *testing.T) {
	term := New()
	term.Resize(geom.Vec2{C: 4, R: 2})
	term.Write([]byte(LineFeedMode))

	// Remove anything in lines that can mess with comparisons
	cleanLines := func(lines []ScreenLine) {
		for _, line := range lines {
			for i := range line.Chars {
				line.Chars[i].Write = 0
				line.Chars[i].Mode = 0
			}
		}
	}

	// Flow an empty screen
	{
		result := term.Flow(
			geom.Vec2{
				R: 2,
				C: 4,
			},
			term.Root(),
		)

		require.True(t, result.OK)
		require.True(t, result.CursorOK)
		require.Equal(t, 2, result.NumLines)
		require.Equal(t, 0, result.Cursor.R)
		require.Equal(t, 0, result.Cursor.C)
	}

	term.Write([]byte("foo\nfoobar\nbaz"))
	// flow should be:
	// 0: foo
	// 1: foob
	// --- the screen starts
	// 1: ar
	// 2: baz
	require.Equal(
		t,
		geom.Vec2{
			R: 1,
			C: 4,
		},
		term.Root(),
	)

	// Just check that the screen is correct
	{
		result := term.Flow(
			geom.Vec2{
				R: 2,
				C: 4,
			},
			term.Root(),
		)

		cleanLines(result.Lines)

		require.True(t, result.OK)
		require.Equal(t, 3, result.NumLines)
		require.Equal(t,
			[]ScreenLine{
				{
					R:  1,
					C0: 4,
					C1: 6,
					Chars: makeLine(
						"ar",
					),
				},
				{
					R:  2,
					C0: 0,
					C1: 3,
					Chars: makeLine(
						"baz",
					),
				},
			},
			result.Lines,
		)

		require.True(t, result.CursorOK)
		require.Equal(t, 1, result.Cursor.R)
		require.Equal(t, 3, result.Cursor.C)
	}

	// Check that negative counts work correctly
	{
		result := term.Flow(
			geom.Vec2{
				R: -2,
				C: 4,
			},
			term.Root(),
		)

		cleanLines(result.Lines)

		require.True(t, result.OK)
		require.Equal(t, 3, result.NumLines)
		require.Equal(t,
			[]ScreenLine{
				{
					R:  0,
					C0: 0,
					C1: 3,
					Chars: makeLine(
						"foo",
					),
				},
				{
					R:  1,
					C0: 0,
					C1: 4,
					Chars: makeLine(
						"foob",
					),
				},
			},
			result.Lines,
		)

		require.False(t, result.CursorOK)
	}

	// Do something really weird
	{
		result := term.Flow(
			geom.Vec2{
				C: 2,
				R: 10,
			},
			geom.Vec2{},
		)

		cleanLines(result.Lines)

		require.True(t, result.OK)
		require.Equal(t, 3, result.NumLines)
		require.Equal(t,
			[]ScreenLine{
				{
					R:  0,
					C0: 0,
					C1: 2,
					Chars: makeLine(
						"fo",
					),
				},
				{
					R:  0,
					C0: 2,
					C1: 3,
					Chars: makeLine(
						"o",
					),
				},
				{
					R:  1,
					C0: 0,
					C1: 2,
					Chars: makeLine(
						"fo",
					),
				},
				{
					R:  1,
					C0: 2,
					C1: 4,
					Chars: makeLine(
						"ob",
					),
				},
				{
					R:  1,
					C0: 4,
					C1: 6,
					Chars: makeLine(
						"ar",
					),
				},
				{
					R:  2,
					C0: 0,
					C1: 2,
					Chars: makeLine(
						"ba",
					),
				},
				{
					R:  2,
					C0: 2,
					C1: 3,
					Chars: makeLine(
						"z",
					),
				},
			},
			result.Lines,
		)

		require.True(t, result.CursorOK)
		require.Equal(t, 6, result.Cursor.R)
		require.Equal(t, 1, result.Cursor.C)
	}

	// Handle the cursor going off the end
	{
		result := term.Flow(
			geom.Vec2{
				C: 3,
				R: 10,
			},
			geom.Vec2{},
		)

		cleanLines(result.Lines)

		require.True(t, result.OK)
		require.True(t, result.CursorOK)
		require.Equal(t, 3, result.Cursor.R)
		require.Equal(t, 2, result.Cursor.C)
	}
}

// TestCJKFlow ensures that lines that include wrapped CJK characters are still
// treated as the same physical line, even if they're not "contiguous". This is
// both a problem with wrapping and with flowing.
func TestCJKFlow(t *testing.T) {
	term := New()
	term.Resize(geom.Size{R: 4, C: 3})
	term.Write([]byte(LineFeedMode))
	term.Write([]byte("fo你好bar"))

	result := term.Flow(geom.Vec2{C: 10, R: 1}, geom.Vec2{})
	require.True(t, result.OK)
	require.Equal(t, 1, len(result.Lines))
	require.Equal(t, 1, result.NumLines)
	line := result.Lines[0]
	// The spaces represent blank cells that are "part" of the double-width glyph
	require.Equal(t, "fo你 好 bar", line.Chars.String())
}

func TestHistoryLine(t *testing.T) {
	term := New()
	term.Resize(geom.Size{R: 2, C: 3})
	term.Write([]byte(LineFeedMode))
	term.Write([]byte("foo\nbar\nbaz"))

	result := term.Flow(geom.Vec2{C: 8, R: 0}, geom.Vec2{})
	require.True(t, result.OK)
	require.Equal(t, 3, len(result.Lines))
	require.Equal(t, 3, result.NumLines)
	require.Equal(t, "foo", result.Lines[0].Chars.String())
	require.Equal(t, "bar", result.Lines[1].Chars.String())
	require.Equal(t, "baz", result.Lines[2].Chars.String())
}

func TestGetLines(t *testing.T) {
	term := New()
	term.Resize(geom.Size{R: 2, C: 3})
	term.Write([]byte(LineFeedMode))
	term.Write([]byte("foobar\nbaz"))

	lines := term.GetLines(0, 2)
	require.Equal(t, "foobar", lines[0].String())
	require.Equal(t, "baz", lines[1].String())
}
