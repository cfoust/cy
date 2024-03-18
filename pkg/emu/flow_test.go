package emu

import (
	"testing"

	"github.com/cfoust/cy/pkg/geom"

	"github.com/stretchr/testify/require"
)

func TestFlowLines(t *testing.T) {
	term := New()
	term.Resize(4, 2)
	term.Write([]byte(LineFeedMode))
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

	// Remove anything in lines that can mess with comparisons
	cleanLines := func(lines []ScreenLine) {
		for _, line := range lines {
			for i := range line.Chars {
				line.Chars[i].Write = 0
				line.Chars[i].Mode = 0
			}
		}
	}

	// 1. Just check that the screen is correct
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
	}

	// 2. Check that negative counts work correctly
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
	}

	// 3. Do something really weird
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
	}
}
