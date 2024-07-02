package motion

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
)

func getCurrentLine(m Movable) (line emu.ScreenLine, ok bool) {
	screen, _, cursor := m.Viewport()
	if cursor.R < 0 || cursor.R >= len(screen) {
		return
	}
	return screen[cursor.R], true
}

func screenLineMotion(getIndex func(
	width int,
	line emu.ScreenLine,
) int) Motion {
	return func(m Movable) {
		_, size, _ := m.Viewport()
		line, ok := getCurrentLine(m)
		if !ok {
			return
		}

		index := geom.Clamp(
			getIndex(size.C, line),
			line.C0,
			geom.Max(
				line.C1-1,
				line.C0,
			),
		)

		// Need to check for CJK
		if index > 0 && line.Chars[index-1].Width() == 2 {
			index--
		}

		m.Goto(geom.Vec2{
			R: line.R,
			C: index,
		})
	}
}

// StartOfScreenLine corresponds to vim's `g0`.
var StartOfScreenLine = screenLineMotion(func(width int, line emu.ScreenLine) int {
	return line.C0
})

// MiddleOfScreenLine corresponds to vim's `gm`.
var MiddleOfScreenLine = screenLineMotion(func(width int, line emu.ScreenLine) int {
	return line.C0 + width/2
})

// FirstNonBlankScreen corresponds to vim's `g^`.
var FirstNonBlankScreen = screenLineMotion(func(width int, line emu.ScreenLine) int {
	first, _ := line.Chars.Whitespace()
	return line.C0 + first
})

// LastNonBlankScreen corresponds to vim's `g<end>`.
var LastNonBlankScreen = screenLineMotion(func(width int, line emu.ScreenLine) int {
	_, last := line.Chars.Whitespace()
	return line.C0 + last
})

// EndOfScreenLine corresponds to vim's `g$`.
var EndOfScreenLine = screenLineMotion(func(width int, line emu.ScreenLine) int {
	return line.C1 - 1
})
