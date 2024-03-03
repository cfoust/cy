package replay

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"

	"github.com/mattn/go-runewidth"
)

// Get the occupancy state of the given line.
func getOccupancy(line emu.Line) []bool {
	occupancy := make([]bool, len(line))
	for i := 0; i < len(line); i++ {
		if line[i].IsEmpty() {
			continue
		}

		// handle wide runes
		r := line[i].Char
		w := runewidth.RuneWidth(r)
		for j := 0; j < w; j++ {
			occupancy[i+j] = true
		}
		i += geom.Max(w-1, 0)
	}

	return occupancy
}

func isLineEmpty(line emu.Line) bool {
	occupancy := getOccupancy(line)

	for _, occupied := range occupancy {
		if occupied {
			return false
		}
	}

	return true
}

// Get the indices of the first and last non-empty cells for the given line.
func getNonWhitespace(line emu.Line) (first, last int) {
	for i := 0; i < len(line); i++ {
		if line[i].IsEmpty() {
			continue
		}

		first = i
		break
	}

	for i := len(line) - 1; i >= 0; i-- {
		if line[i].IsEmpty() {
			continue
		}

		last = i
		break
	}

	return
}


// Given a point in term space representing a desired cursor position, return
// the best available cursor position. This enables behavior akin to moving up
// and down in a text editor.
func (r *Replay) resolveDesiredColumn(point geom.Vec2) int {
	line := r.getLine(point.R)
	if line == nil {
		return 0
	}

	occupancy := getOccupancy(line)

	// desiredCol occupied -> return that col
	if occupancy[point.C] {
		return point.C
	}

	var haveBefore, haveAfter bool
	// check for occupied cells before and after the desired column
	for i := 0; i < len(line); i++ {
		if i == point.C || !occupancy[i] {
			continue
		}

		if i > point.C {
			haveAfter = true
		} else {
			haveBefore = true
		}
	}

	// the line is empty, just go to col 0
	if !haveBefore && !haveAfter {
		return 0
	}

	// point.C is before last non-whitespace and after first
	// non-whitespace: remain in place
	if haveBefore && haveAfter {
		return point.C
	}

	// first non-whitespace is after point.C: last column before first
	// non-whitespace
	if haveAfter && !haveBefore {
		first, _ := getNonWhitespace(line)
		return geom.Max(first-1, 0)
	}

	// last non-whitespace is before point.C: last non-whitespace column
	if haveBefore && !haveAfter {
		_, last := getNonWhitespace(line)
		return last
	}

	return 0
}

// Move the cursor to a point in term space, adjusting the viewport the minimum
// amount necessary to keep the cursor in view.
func (r *Replay) moveCursor(point geom.Vec2) {
	viewport := r.viewport
	newCursor := r.termToViewport(point)

	if newCursor.C < 0 {
		r.setOffsetX(r.offset.C + newCursor.C)
	}

	if newCursor.C >= viewport.C {
		r.setOffsetX(r.offset.C + (newCursor.C - viewport.C + 1))
	}

	if newCursor.R < 0 {
		r.setOffsetY(r.offset.R + newCursor.R)
	}

	if newCursor.R >= viewport.R {
		r.setOffsetY(r.offset.R + (newCursor.R - viewport.R + 1))
	}

	r.cursor = r.termToViewport(point)
}

// Attempt to move the cursor relative to its current position. Sets
// `desiredCol` if the motion is horizontal.
func (r *Replay) moveCursorDelta(dy, dx int) {
	oldPos := r.viewportToTerm(r.cursor)
	newPos := r.clampToTerminal(oldPos.Add(geom.Vec2{
		R: dy,
		C: dx,
	}))

	// Don't allow user to move onto blank lines at end of terminal
	numBlank := 0
	screen := r.player.Screen()
	termSize := r.getTerminalSize()
	for row := termSize.R - 1; row >= 0; row-- {
		if !isLineEmpty(screen[row]) {
			break
		}
		numBlank++
	}
	newPos.R = geom.Min(termSize.R-1-numBlank, newPos.R)

	// Don't do anything if we can't move
	if newPos == oldPos {
		return
	}

	// Motion to the right is bounded to the last non-whitespace character
	if newPos.C > oldPos.C {
		line := r.getLine(newPos.R)
		if line == nil {
			return
		}
		_, lastCell := getNonWhitespace(line)
		newPos.C = geom.Min(lastCell, newPos.C)
	}

	if newPos.R != oldPos.R && newPos.C == oldPos.C {
		newPos.C = r.resolveDesiredColumn(geom.Vec2{
			R: newPos.R,
			C: r.desiredCol,
		})
	}

	r.mode = ModeCopy
	r.moveCursor(newPos)

	// If the column motion was intentional, set the desiredCol
	if newPos.C != oldPos.C && dx != 0 {
		r.desiredCol = r.termToViewport(newPos).C
	}
}
