package motion

import (
	"testing"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"

	"github.com/stretchr/testify/require"
)

type TestMovable struct {
	cursor geom.Vec2
	lines  []emu.Line
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

	return m.lines[row], true
}

func fromLine(text string) Movable {
	line := emu.LineFromString(text)
	return &TestMovable{
		lines: []emu.Line{line},
	}
}

func TestJump(t *testing.T) {
	m := fromLine("The five boxing wizards jump quickly. a")
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
