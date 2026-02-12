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
var StartOfScreenLine = screenLineMotion(
	func(width int, line emu.ScreenLine) int {
		return line.C0
	},
)

// MiddleOfScreenLine corresponds to vim's `gm`.
var MiddleOfScreenLine = screenLineMotion(
	func(width int, line emu.ScreenLine) int {
		return line.C0 + width/2
	},
)

// FirstNonBlankScreen corresponds to vim's `g^`.
var FirstNonBlankScreen = screenLineMotion(
	func(width int, line emu.ScreenLine) int {
		first, _ := line.Chars.Whitespace()
		return line.C0 + first
	},
)

// LastNonBlankScreen corresponds to vim's `g<end>`.
var LastNonBlankScreen = screenLineMotion(
	func(width int, line emu.ScreenLine) int {
		_, last := line.Chars.Whitespace()
		return line.C0 + last
	},
)

// EndOfScreenLine corresponds to vim's `g$`.
var EndOfScreenLine = screenLineMotion(
	func(width int, line emu.ScreenLine) int {
		return line.C1 - 1
	},
)

func screenPosition(m Movable, getRow func(numLines int) int) {
	screen, _, _ := m.Viewport()
	if len(screen) == 0 {
		return
	}

	targetRow := getRow(len(screen))

	// If target row has no content, find the closest row that does
	if len(screen[targetRow].Chars) == 0 {
		// Search outward from target to find closest content line
		for delta := 1; delta < len(screen); delta++ {
			// Check above
			if targetRow-delta >= 0 && len(screen[targetRow-delta].Chars) > 0 {
				targetRow = targetRow - delta
				break
			}
			// Check below
			if targetRow+delta < len(screen) &&
				len(screen[targetRow+delta].Chars) > 0 {
				targetRow = targetRow + delta
				break
			}
		}
	}

	// Still no content found
	if len(screen[targetRow].Chars) == 0 {
		return
	}

	line := screen[targetRow]
	first, _ := line.Chars.Whitespace()
	m.Goto(geom.Vec2{
		R: line.R,
		C: line.C0 + first,
	})
}

// ScreenTop corresponds to vim's `H` (jump to top of screen).
func ScreenTop(m Movable) {
	screenPosition(m, func(n int) int { return 0 })
}

// ScreenMiddle corresponds to vim's `M` (jump to middle of screen).
func ScreenMiddle(m Movable) {
	screenPosition(m, func(n int) int { return n / 2 })
}

// ScreenBottom corresponds to vim's `L` (jump to bottom of screen).
func ScreenBottom(m Movable) {
	screenPosition(m, func(n int) int { return n - 1 })
}

func cursorScreenLine(m Movable, delta int) {
	screen, _, cursor := m.Viewport()
	targetRow := cursor.R + delta

	if len(screen) == 0 || targetRow < 0 || targetRow >= len(screen) {
		return
	}

	currentLine := screen[cursor.R]
	targetLine := screen[targetRow]

	cursorPos := m.Cursor()
	visualCol := cursorPos.C - currentLine.C0

	newCol := geom.Clamp(
		targetLine.C0+visualCol,
		targetLine.C0,
		targetLine.C1-1,
	)

	m.Goto(geom.Vec2{
		R: targetLine.R,
		C: newCol,
	})
}

// CursorDownScreen corresponds to vim's `gj` (move down by visual line).
func CursorDownScreen(m Movable) {
	cursorScreenLine(m, 1)
}

// CursorUpScreen corresponds to vim's `gk` (move up by visual line).
func CursorUpScreen(m Movable) {
	cursorScreenLine(m, -1)
}
