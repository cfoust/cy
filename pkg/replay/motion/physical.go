package motion

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"

	"github.com/mattn/go-runewidth"
)

func lineMotion(getIndex func(line emu.Line) int) Motion {
	return func(m Movable) {
		cursor := m.Cursor()
		line, ok := m.Line(cursor.R)
		if !ok {
			return
		}

		index := geom.Clamp(
			getIndex(line),
			0,
			geom.Max(len(line)-1, 0),
		)

		// Need to check for CJK
		if index > 0 && runewidth.RuneWidth(line[index-1].Char) == 2 {
			index--
		}

		m.Goto(geom.Vec2{
			R: cursor.R,
			C: index,
		})
	}
}

// StartOfLine corresponds to vim's "0".
var StartOfLine = lineMotion(func(line emu.Line) int {
	return 0
})

// MiddleOfLine corresponds to vim's "gM".
var MiddleOfLine = lineMotion(func(line emu.Line) int {
	return len(line) / 2
})

// EndOfLine corresponds to vim's "$".
var EndOfLine = lineMotion(func(line emu.Line) int {
	return len(line) - 1
})

// FirstNonBlank corresponds to vim's "^".
var FirstNonBlank = lineMotion(func(line emu.Line) int {
	first, _ := line.Whitespace()
	return first
})

// LastNonBlank corresponds to vim's "g_".
var LastNonBlank = lineMotion(func(line emu.Line) int {
	_, last := line.Whitespace()
	return last
})
