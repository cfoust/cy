package motion

import (
	"github.com/cfoust/cy/pkg/geom"

	"github.com/mattn/go-runewidth"
)

// StartOfLine corresponds to vim's "0".
func StartOfLine(m Movable) {
	cursor := m.Cursor()
	m.Goto(geom.Vec2{R: cursor.R, C: 0})
}

// FirstNonBlank corresponds to vim's "^".
func FirstNonBlank(m Movable) {
	cursor := m.Cursor()
	line, ok := m.Line(cursor.R)
	if !ok {
		return
	}
	first, _ := line.Whitespace()
	m.Goto(geom.Vec2{R: cursor.R, C: first})
}

// EndOfLine corresponds to vim's "$".
func EndOfLine(m Movable) {
	cursor := m.Cursor()
	line, ok := m.Line(cursor.R)
	if !ok {
		return
	}

	index := len(line) - 1

	// Need to check for CJK
	if len(line) > 1 && runewidth.RuneWidth(line[index-1].Char) == 2 {
		index--
	}

	m.Goto(geom.Vec2{R: cursor.R, C: index})
}

// LastNonBlank corresponds to vim's "g_".
func LastNonBlank(m Movable) {
	cursor := m.Cursor()
	line, ok := m.Line(cursor.R)
	if !ok {
		return
	}
	_, last := line.Whitespace()
	m.Goto(geom.Vec2{R: cursor.R, C: last})
}
