package emu

import (
	"testing"

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
	term.Resize(2, 4)
	term.Write([]byte(LineFeedMode))
	term.Write([]byte("foo\nfoobar\nbaz"))
	// should be:
	// 0: foo
	// 1: foob
	// ---
	// 1: ar
	// 2: baz

	//lines, ok := term.Flow(
	//geom.Vec2{R: 2, C: 4},
	//geom.Vec2{R: 1},
	//-2,
	//)
	//t.Logf("%+v %+v", lines, ok)
	//t.Fail()
}
