package movement

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"

	"github.com/mattn/go-runewidth"
)

type imageMovement struct {
	emu.Terminal

	cursor geom.Vec2

	// The [R, C] offset of the viewport relative to the top-left corner of
	// the underlying terminal.
	offset, minOffset, maxOffset, viewport geom.Vec2
}

var _ Movement = (*imageMovement)(nil)

func NewImage(terminal emu.Terminal) Movement {
	i := &imageMovement{Terminal: terminal}

	termCursor := getTerminalCursor(i.Terminal)
	viewportCursor := i.termToViewport(termCursor)

	// Center the cursor if it's not in the viewport
	if !i.isInViewport(viewportCursor) {
		i.centerPoint(termCursor)
	}

	i.cursor = i.termToViewport(termCursor)

	return i
}

func (i *imageMovement) ScrollTop() {
	i.MoveCursorY(
		-i.viewportToTerm(i.cursor).R + i.minOffset.R,
	)
}

func (i *imageMovement) ScrollBottom() {
	i.MoveCursorY(
		(getTerminalSize(i.Terminal).R - 1) - i.viewportToTerm(i.cursor).R,
	)
}

func (i *imageMovement) Reset() {
	termCursor := getTerminalCursor(i.Terminal)
	i.centerPoint(termCursor)
	i.cursor = i.termToViewport(termCursor)
}

// Check whether the given point in viewport space actually falls within it.
func (i *imageMovement) isInViewport(point geom.Vec2) bool {
	if point.R < 0 || point.C < 0 || point.R >= i.viewport.R || point.C >= i.viewport.C {
		return false
	}

	return true
}

// Ensure a point in term space falls inside of the terminal and its scrollback.
func (i *imageMovement) clampToTerminal(point geom.Vec2) geom.Vec2 {
	return point.Clamp(
		i.minOffset,
		getTerminalSize(i.Terminal).Sub(geom.UnitVec2),
	)
}

// Center the viewport on a point in the reference frame of the terminal.
func (i *imageMovement) centerPoint(point geom.Vec2) {
	i.setOffsetX(point.C - (i.viewport.C / 2))
	i.setOffsetY(point.R - (i.viewport.R / 2))
}

// Translate a coordinate in the reference frame of the terminal to a point in
// the viewport.
func (i *imageMovement) termToViewport(point geom.Vec2) geom.Vec2 {
	return point.Sub(i.offset)
}

func (i *imageMovement) viewportToTerm(point geom.Vec2) geom.Vec2 {
	return point.Add(i.offset)
}

func (i *imageMovement) setOffset(offset geom.Vec2) {
	cursor := i.viewportToTerm(i.cursor)
	i.offset = i.offset.
		Add(offset).
		Clamp(i.minOffset, i.maxOffset)
	i.cursor = i.termToViewport(cursor)
}

func (i *imageMovement) setOffsetY(offset int) {
	i.setOffset(geom.Vec2{R: offset})
}

func (i *imageMovement) setOffsetX(offset int) {
	i.setOffset(geom.Vec2{C: offset})
}

// Get the glyphs for a row in term space.
func (i *imageMovement) getLine(row int) emu.Line {
	screen := i.Screen()
	history := i.History()

	// Handle out-of-bounds lines
	clamped := geom.Clamp(
		row,
		-len(history),
		getTerminalSize(i.Terminal).R-1,
	)
	if clamped != row {
		return nil
	}

	var line emu.Line
	if row < 0 {
		line = history[len(history)+row]
	} else {
		line = screen[row]
	}

	return line
}

// Move the cursor to a point in term space, adjusting the viewport the minimum
// amount necessary to keep the cursor in view.
func (i *imageMovement) moveCursor(point geom.Vec2) {
	viewport := i.viewport
	newCursor := i.termToViewport(point)

	if newCursor.C < 0 {
		i.setOffsetX(i.offset.C + newCursor.C)
	}

	if newCursor.C >= viewport.C {
		i.setOffsetX(i.offset.C + (newCursor.C - viewport.C + 1))
	}

	if newCursor.R < 0 {
		i.setOffsetY(i.offset.R + newCursor.R)
	}

	if newCursor.R >= viewport.R {
		i.setOffsetY(i.offset.R + (newCursor.R - viewport.R + 1))
	}

	i.cursor = newCursor
}

func (i *imageMovement) moveCursorDelta(delta geom.Vec2) {
	oldPos := i.viewportToTerm(i.cursor)
	newPos := i.clampToTerminal(oldPos.Add(delta))
	i.moveCursor(newPos)
}

func (i *imageMovement) ScrollYDelta(delta int) {
	before := i.viewportToTerm(i.cursor)
	i.setOffsetY(i.offset.R + delta)
	i.cursor = i.termToViewport(before).Clamp(
		geom.Vec2{},
		geom.Vec2{
			R: i.viewport.R - 1,
			C: i.viewport.C - 1,
		},
	)
}

// Calculate the bounds of `{min,max}Offset` and ensure `offset` falls between them.
func (i *imageMovement) recalculateViewport() {
	termSize := getTerminalSize(i.Terminal)
	i.minOffset = geom.Vec2{
		R: -len(i.History()),
		C: 0, // always, but for clarity
	}
	i.maxOffset = geom.Vec2{
		R: geom.Max(termSize.R-i.viewport.R, 0),
		C: geom.Max(termSize.C-i.viewport.C, 0),
	}
	i.setOffsetY(i.offset.R)
	i.setOffsetX(i.offset.C)
}

func (i *imageMovement) setScrollX(offset int) {
	before := i.viewportToTerm(i.cursor)
	i.setOffsetX(offset)
	after := i.termToViewport(before)

	if after.C >= i.viewport.C {
		i.cursor.C = geom.Max(i.viewport.C-1, 0)
	} else if after.C < 0 {
		i.cursor.C = 0
	} else {
		i.cursor.C = after.C
	}
}

func (i *imageMovement) ScrollXDelta(delta int) {
	// TODO(cfoust): 03/25/24
}

func normalizeRange(start, end geom.Vec2) (newStart, newEnd geom.Vec2) {
	if end.R < start.R || (end.R == start.R && end.C < start.C) {
		intermediate := start
		start = end
		end = intermediate
	}

	return start, end
}

func (i *imageMovement) Cursor() geom.Vec2 {
	return i.viewportToTerm(i.cursor)
}

// Read a starting from `start` to `end`, inclusive.
func (i *imageMovement) ReadString(start, end geom.Vec2) (result string) {
	start, end = normalizeRange(start, end)

	var char rune
	var startCol, endCol, lastChar int
	for row := start.R; row <= end.R; row++ {
		line := i.getLine(row)
		startCol = 0
		if row == start.R {
			startCol = start.C
		}

		_, lastChar = getNonWhitespace(line)
		endCol = lastChar
		if row == end.R {
			endCol = geom.Min(end.C, endCol)
		}

		for col := startCol; col <= endCol; col++ {
			char = line[col].Char
			result += string(char)

			w := runewidth.RuneWidth(char)
			for i := 1; i < w; i++ {
				col++
			}
		}

		if row != end.R && endCol == lastChar {
			result += "\n"
		}
	}

	return
}

func (i *imageMovement) Resize(size geom.Vec2) {
	i.viewport = size
	i.recalculateViewport()
	i.setOffsetY(-1)
	i.centerPoint(i.cursor)
}

func (i *imageMovement) MoveCursorX(delta int) {
	i.moveCursorDelta(geom.Vec2{C: delta})
}

func (i *imageMovement) MoveCursorY(delta int) {
	i.moveCursorDelta(geom.Vec2{R: delta})
}

func (f *imageMovement) Jump(needle string, isForward bool, isTo bool) {
	oldPos := f.viewportToTerm(f.cursor)
	line := f.getLine(oldPos.R)
	f.moveCursor(geom.Vec2{
		R: oldPos.R,
		C: calculateJump(line, needle, isForward, isTo, oldPos.C),
	})
}
