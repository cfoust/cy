package image

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/params"
	"github.com/cfoust/cy/pkg/replay/movement"
	"github.com/cfoust/cy/pkg/style"
)

type imageMovement struct {
	emu.Terminal

	cursor geom.Vec2

	// The [R, C] offset of the viewport relative to the top-left corner of
	// the underlying terminal.
	offset, minOffset, maxOffset, viewport geom.Vec2
}

var _ movement.Movement = (*imageMovement)(nil)

func getTerminalCursor(terminal emu.Terminal) geom.Vec2 {
	cursor := terminal.Cursor()
	return geom.Vec2{
		R: cursor.R,
		C: cursor.C,
	}
}

func New(terminal emu.Terminal, viewport geom.Size) movement.Movement {
	i := &imageMovement{Terminal: terminal}

	i.viewport = viewport
	i.recalculateViewport()

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
		(i.Terminal.Size().R - 1) - i.viewportToTerm(i.cursor).R,
	)
}

func (i *imageMovement) Snap() {
	// no-op in imageMovement
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
		i.Terminal.Size().Sub(geom.UnitVec2),
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
	i.offset = offset.Clamp(i.minOffset, i.maxOffset)
	i.cursor = i.termToViewport(cursor)
}

func (i *imageMovement) setOffsetY(offset int) {
	i.setOffset(geom.Vec2{
		R: offset,
		C: i.offset.C,
	})
}

func (i *imageMovement) setOffsetX(offset int) {
	i.setOffset(geom.Vec2{
		R: i.offset.R,
		C: offset,
	})
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

	// After the viewport has moved, set the cursor based on its absolute
	// position
	i.cursor = i.termToViewport(point)
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
	termSize := i.Terminal.Size()
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
	before := i.viewportToTerm(i.cursor)
	i.setOffsetX(i.offset.C + delta)
	i.cursor = i.termToViewport(before).Clamp(
		geom.Vec2{},
		geom.Vec2{
			R: i.viewport.R - 1,
			C: i.viewport.C - 1,
		},
	)
}

// Read a starting from `start` to `end`, inclusive.
func (i *imageMovement) ReadString(start, end geom.Vec2) (result string) {
	start = i.clampToTerminal(start)
	end = i.clampToTerminal(end)
	start, end = geom.NormalizeRange(start, end)
	start, end = normalizeBoxRange(start, end)

	screen := i.Screen()
	for row := start.R; row <= end.R; row++ {
		line := screen[row][start.C : end.C+1]

		if line.Length() == 0 {
			result += "\n"
			continue
		}

		_, lastChar := line.Whitespace()
		result += line[:lastChar+1].String()
		if row != end.R {
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

////func (i *imageMovement) Jump(needle string, isForward bool, isTo bool) {
////oldPos := i.viewportToTerm(i.cursor)
////line := i.getLine(oldPos.R)
////i.moveCursor(geom.Vec2{
////R: oldPos.R,
////C: calculateJump(line, needle, isForward, isTo, oldPos.C),
////})
////}

// For a point that is off the screen, find the closest point that can be used
// as the start or end point of a selection.
func anchorToScreen(size geom.Vec2, v geom.Vec2) geom.Vec2 {
	if v.R < 0 {
		return geom.Vec2{}
	}

	if v.R >= size.R {
		return geom.Vec2{
			R: size.R - 1,
			C: size.C - 1,
		}
	}

	if v.C < 0 {
		return geom.Vec2{
			R: v.R,
			C: 0,
		}
	}

	if v.C > 0 {
		return geom.Vec2{
			R: v.R,
			C: size.C - 1,
		}
	}

	return v
}

func (i *imageMovement) highlightRange(state *tty.State, from, to geom.Vec2, fg, bg emu.Color) {
	from, to = geom.NormalizeRange(from, to)
	from = i.termToViewport(from)
	to = i.termToViewport(to)

	size := state.Image.Size()
	if !i.isInViewport(from) {
		from = anchorToScreen(size, from)
	}
	if !i.isInViewport(to) {
		to = anchorToScreen(size, to)
	}

	var startCol, endCol int
	for row := from.R; row <= to.R; row++ {
		startCol = 0
		if row == from.R {
			startCol = from.C
		}

		endCol = size.C - 1
		if row == to.R {
			endCol = to.C
		}

		for col := startCol; col <= endCol; col++ {
			state.Image[row][col].FG = fg
			state.Image[row][col].BG = bg
		}
	}
}

// normalizeBoxRange normalizes box selections so that `from` is the top-left
// coordinate and `to` is the bottom right coordinate defining the box.
func normalizeBoxRange(from, to geom.Vec2) (newFrom, newTo geom.Vec2) {
	newFrom = geom.Vec2{
		R: geom.Min(from.R, to.R),
		C: geom.Min(from.C, to.C),
	}
	newTo = geom.Vec2{
		R: geom.Max(from.R, to.R),
		C: geom.Max(from.C, to.C),
	}
	return
}

func (i *imageMovement) highlightRow(
	row emu.Line,
	start, end geom.Vec2,
	highlight movement.Highlight,
) {
	var (
		from = highlight.From
		to   = highlight.To
	)

	if to.R < start.R || from.R > end.R {
		return
	}

	// Non-Screen highlights are just boxes
	if !highlight.Screen {
		if to.C < start.C {
			return
		}

		if from.C > end.C {
			return
		}
	}

	var startCol, endCol int

	lastIndex := len(row) - 1

	if !highlight.Screen {
		startCol = from.C - start.C
		endCol = to.C - start.C
	} else {
		if from.R == start.R {
			startCol = from.C - start.C
			endCol = lastIndex
		}

		if to.R == end.R {
			endCol = to.C - start.C
		}

		if from.R < start.R && to.R > end.R {
			startCol = 0
			endCol = lastIndex
		}
	}

	if startCol > endCol {
		return
	}

	startCol = geom.Clamp(startCol, 0, lastIndex)
	endCol = geom.Clamp(endCol, 0, lastIndex)

	for col := startCol; col <= endCol; col++ {
		row[col].FG = highlight.FG
		row[col].BG = highlight.BG
	}
}

func (i *imageMovement) View(
	params *params.Parameters,
	state *tty.State,
	highlights []movement.Highlight,
) {
	for i, highlight := range highlights {
		var (
			from = highlight.From
			to   = highlight.To
		)
		from, to = geom.NormalizeRange(from, to)

		if !highlight.Screen {
			from, to = normalizeBoxRange(from, to)
		}

		highlight.From = from
		highlight.To = to
		highlights[i] = highlight
	}

	screen := i.Screen()
	termSize := i.Terminal.Size()
	var point geom.Vec2
	var glyph emu.Glyph
	var start, end geom.Vec2
	image := state.Image
	for row := 0; row < i.viewport.R; row++ {
		point.R = i.offset.R + row

		start = geom.Vec2{
			R: point.R,
			C: i.offset.C,
		}
		end = geom.Vec2{
			R: point.R,
			C: geom.Min(
				i.offset.C+i.viewport.C-1,
				termSize.C-1,
			),
		}

		for col := 0; col < i.viewport.C; col++ {
			point.C = i.offset.C + col

			if point.C >= termSize.C || point.R >= termSize.R {
				glyph = emu.EmptyGlyph()
				glyph.FG = 8
				glyph.Char = '-'
			} else {
				glyph = screen[point.R][point.C]
			}

			image[row][col] = glyph
		}

		for _, highlight := range highlights {
			i.highlightRow(image[row], start, end, highlight)
		}
	}

	termCursor := i.termToViewport(getTerminalCursor(i.Terminal))
	if i.cursor != termCursor {
		state.Cursor.Vec2 = i.cursor

		// In copy mode, leave behind a ghost cursor where the
		// terminal's cursor is
		if i.isInViewport(termCursor) {
			style.GhostCursor(
				state.Image,
				termCursor.R,
				termCursor.C,
			)
		}
	} else {
		state.Cursor = i.Terminal.Cursor()
		state.Cursor.Vec2 = termCursor
	}
}
