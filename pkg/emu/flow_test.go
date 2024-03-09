package emu

import (
	"testing"

	"github.com/cfoust/cy/pkg/geom"

	"github.com/stretchr/testify/require"
)

// Dangerously close to makeBreakfast.
func makeBreakTest(
	t *testing.T,
	line Line,
	cols int,
	lines ...string,
) {
	brokenLines := breakLine(line, 0, 0, cols)
	if len(brokenLines) != len(lines) {
		t.Errorf("expected %d lines, got %d", len(lines), len(brokenLines))
		return
	}

	for i, broken := range brokenLines {
		require.Equal(t, makeLine(lines[i]), broken.Chars)
	}
}

func TestBreakLine(t *testing.T) {
	xiyouji := makeLine("混沌未分天地亂")

	makeBreakTest(t, xiyouji, 4,
		"混沌",
		"未分",
		"天地",
		"亂",
	)

	makeBreakTest(t, xiyouji, 3,
		"混",
		"沌",
		"未",
		"分",
		"天",
		"地",
		"亂",
	)

	makeBreakTest(t, xiyouji, 2,
		"混",
		"沌",
		"未",
		"分",
		"天",
		"地",
		"亂",
	)

	// cuts off trailing characters
	english := makeLine("fo barbaz ")
	makeBreakTest(t, english, 3,
		"fo ",
		"bar",
		"baz",
	)
}

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
	cleanLines := func(lines []FlowLine) {
		for _, line := range lines {
			for i := range line.Chars {
				line.Chars[i].Write = 0
				line.Chars[i].Mode = 0
			}
		}
	}

	// 1. Just check that the screen is correct
	{
		lines, ok := term.Flow(
			geom.Vec2{
				R: 2,
				C: 4,
			},
			term.Root(),
			2,
		)

		cleanLines(lines)

		require.True(t, ok)
		require.Equal(t,
			[]FlowLine{
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
			lines,
		)
	}

	// 2. Check that negative counts work correctly
	{
		lines, ok := term.Flow(
			geom.Vec2{
				R: 2,
				C: 4,
			},
			term.Root(),
			-2,
		)

		cleanLines(lines)

		require.True(t, ok)
		require.Equal(t,
			[]FlowLine{
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
			lines,
		)
	}

	// 3. Do something really weird
	{
		lines, ok := term.Flow(
			geom.Vec2{C: 2},
			geom.Vec2{},
			10,
		)

		cleanLines(lines)

		require.True(t, ok)
		require.Equal(t,
			[]FlowLine{
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
			lines,
		)
	}
}
