package motion

import (
	"testing"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"

	"github.com/stretchr/testify/require"
)

type TestMovable struct {
	cursor geom.Vec2
	size   geom.Vec2
	lines  []emu.ScreenLine
}

var _ Movable = (*TestMovable)(nil)

func (m *TestMovable) Cursor() geom.Vec2 {
	return m.cursor
}

func (m *TestMovable) Goto(location geom.Vec2) {
	m.cursor = location
}

func (m *TestMovable) Line(row int) (line emu.Line, ok bool) {
	if row < 0 || row >= len(m.lines) {
		return
	}

	return m.lines[row].Chars, true
}

func (m *TestMovable) Viewport() (
	screen []emu.ScreenLine,
	cursor geom.Vec2,
) {
	return m.lines, m.cursor
}

func fromLines(lines ...string) *TestMovable {
	var screenLines []emu.ScreenLine
	width := 0
	for i, line := range lines {
		chars := emu.LineFromString(line)
		width = geom.Max(width, len(chars))
		screenLines = append(
			screenLines,
			emu.ScreenLine{
				R:     i,
				C1:    len(chars),
				Chars: chars,
			},
		)
	}

	return &TestMovable{
		size: geom.Vec2{
			R: len(screenLines),
			C: width,
		},
		lines: screenLines,
	}
}

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
