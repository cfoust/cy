package motion

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
)

type testMovable struct {
	cursor geom.Vec2
	size   geom.Vec2
	lines  []emu.ScreenLine
}

var _ Movable = (*testMovable)(nil)

func (m *testMovable) Cursor() geom.Vec2 {
	return m.cursor
}

func (m *testMovable) Goto(location geom.Vec2) {
	m.cursor = location
}

func (m *testMovable) Line(row int) (line emu.Line, ok bool) {
	if row < 0 || row >= len(m.lines) {
		return
	}

	return m.lines[row].Chars, true
}

func (m *testMovable) Viewport() (
	screen []emu.ScreenLine,
	size geom.Vec2,
	cursor geom.Vec2,
) {
	return m.lines, m.size, m.cursor
}

func fromLines(lines ...string) *testMovable {
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

	return &testMovable{
		size: geom.Vec2{
			R: len(screenLines),
			C: width,
		},
		lines: screenLines,
	}
}
