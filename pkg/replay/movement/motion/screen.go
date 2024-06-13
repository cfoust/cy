package motion

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"

	"github.com/mattn/go-runewidth"
)

func getCurrentLine(m Movable) (line emu.ScreenLine, ok bool) {
	screen, _, cursor := m.Viewport()
	if cursor.R < 0 || cursor.R >= len(screen) {
		return
	}
	return screen[cursor.R], true
}

func screenLineMotion(getIndex func(
	m Movable,
	line emu.ScreenLine,
) int) Motion {
	return func(m Movable) {
		line, ok := getCurrentLine(m)
		if !ok {
			return
		}

		index := geom.Clamp(
			getIndex(m, line),
			line.C0,
			geom.Max(
				line.C1-1,
				line.C0,
			),
		)

		// Need to check for CJK
		if index > 0 && runewidth.RuneWidth(
			line.Chars[index-1].Char,
		) == 2 {
			index--
		}

		m.Goto(geom.Vec2{
			R: line.R,
			C: index,
		})
	}
}

// StartOfScreenLine corresponds to vim's `g0`.
var StartOfScreenLine = screenLineMotion(func(m Movable, line emu.ScreenLine) int {
	return line.C0
})

// MiddleOfScreenLine corresponds to vim's `gm`.
var MiddleOfScreenLine = screenLineMotion(func(m Movable, line emu.ScreenLine) int {
	_, size, _ := m.Viewport()
	return line.C0 + size.C/2
})
