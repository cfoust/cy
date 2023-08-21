package emu

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func makeLine(text string) Line {
	line := make(Line, 0)

	for _, r := range text {
		glyph := EmptyGlyph()
		glyph.Char = r
		line = append(line, glyph)
	}

	return line
}

func makeWrapped(lines ...string) []Line {
	result := make([]Line, 0)
	for i, str := range lines {
		line := makeLine(str)
		if i != len(lines)-1 {
			line[len(line)-1].Mode |= attrWrap
		}
		result = append(result, line)
	}
	return result
}

func ensureWrap(t *testing.T, input string, cols int, expected []Line) {
	result := wrapLine(makeLine(input), cols)
	require.Equal(t, expected, result)
}

func TestWrap(t *testing.T) {
	ensureWrap(t, "a a", 2, makeWrapped(
		"a ",
		"a ",
	))
	ensureWrap(t, "bbbb", 2, makeWrapped(
		"bb",
		"bb",
	))
	ensureWrap(t, "a   ", 2, makeWrapped(
		"a ",
	))
}

func TestTerm(t *testing.T) {
	term := New()

	// 1. One long line
	for i := 0; i < 40; i++ {
		term.Write([]byte("a"))
	}

	for i := 0; i < 40; i++ {
		term.Write([]byte("b"))
	}

	term.Resize(40, 24)
	require.Equal(t, "a", extractStr(term, 39, 39, 0))
	require.Equal(t, "b", extractStr(term, 0, 0, 1))
	term.Resize(80, 24)
	require.Equal(t, "b", extractStr(term, 40, 40, 0))

	// 2. Several lines
	term = New()
	term.Write([]byte("\033[20h")) // set CRLF mode
	term.Write([]byte("test\ntest"))
	term.Resize(40, 24)
	require.Equal(t, "test", extractStr(term, 0, 3, 0))
	require.Equal(t, "test", extractStr(term, 0, 3, 1))
}
