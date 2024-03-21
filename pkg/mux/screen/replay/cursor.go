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

func resolveDesiredColumn(line emu.Line, col int) int {
	occupancy := getOccupancy(line)
	if col > len(occupancy) {
		return len(occupancy) - 1
	}

	// desiredCol occupied -> return that col
	if occupancy[col] {
		return col
	}

	var haveBefore, haveAfter bool
	// check for occupied cells before and after the desired column
	for i := 0; i < len(line); i++ {
		if i == col || !occupancy[i] {
			continue
		}

		if i > col {
			haveAfter = true
		} else {
			haveBefore = true
		}
	}

	// the line is empty, just go to col 0
	if !haveBefore && !haveAfter {
		return 0
	}

	// col is before last non-whitespace and after first
	// non-whitespace: remain in place
	if haveBefore && haveAfter {
		return col
	}

	// first non-whitespace is after col: last column before first
	// non-whitespace
	if haveAfter && !haveBefore {
		first, _ := getNonWhitespace(line)
		return geom.Max(first-1, 0)
	}

	// last non-whitespace is before col: last non-whitespace column
	if haveBefore && !haveAfter {
		_, last := getNonWhitespace(line)
		return last
	}

	return 0
}

// Given a point in term space representing a desired cursor position, return
// the best available cursor position. This enables behavior akin to moving up
// and down in a text editor.
func (r *Replay) resolveScreenColumn(row int) int {
	result := r.Flow(r.viewport, r.root)
	if !result.OK {
		return 0
	}

	lines := result.Lines
	if row < 0 || row >= len(lines) {
		return 0
	}

	return resolveDesiredColumn(lines[row].Chars, r.desiredCol)
}

func (r *Replay) moveCursorX(delta int) {
	if r.isImageMode() {
		r.moveCursorDelta(geom.Vec2{C: delta})
		return
	}

	current, ok := r.getFlowLine(r.cursor.R)
	if !ok {
		return
	}

	oldCol := r.cursor.C
	newCol := geom.Clamp(
		r.cursor.C+delta,
		0,
		len(current.Chars)-1,
	)

	// Motion to the right is bounded by the last non-whitespace character
	if newCol > oldCol {
		_, lastCell := getNonWhitespace(current.Chars)
		newCol = geom.Min(lastCell, newCol)
	}

	// Don't do anything if we can't move
	if newCol == oldCol {
		return
	}

	r.mode = ModeCopy
	r.cursor.C = newCol
	r.desiredCol = newCol
}

// moveCursorDelta attempts to move the cursor relative to its current
// position. Sets `desiredCol` if the motion is horizontal (ie dx != 0).
func (r *Replay) moveCursorY(delta int) {
	// We only do a simple bounds check in image mode
	if r.isImageMode() {
		r.moveCursorDelta(geom.Vec2{R: delta})
		return
	}

	// Don't allow user to move onto blank lines at end of terminal
	screen := r.Flow(r.getTerminalSize(), r.Root())
	lastLine := screen.Lines[len(screen.Lines)-1].Root()
	for row := len(screen.Lines) - 1; row >= 0; row-- {
		if !isLineEmpty(screen.Lines[row].Chars) {
			break
		}
		lastLine = screen.Lines[row].Root()
	}

	current, ok := r.getFlowLine(r.cursor.R)
	if !ok {
		return
	}

	numRows := delta
	if delta >= 0 {
		// Include the root line
		numRows++
	}

	// We want to flow from the current line of the cursor to its
	// destination so that we can determine how much to move the viewport
	// and where to leave the cursor.
	flow := r.Flow(geom.Vec2{
		R: numRows,
		C: r.viewport.C,
	}, geom.Vec2{
		R: current.R,
		C: current.C0,
	})
	if !flow.OK {
		return
	}

	// Ensure the user can't move past the last line
	for i := 0; i < len(flow.Lines); i++ {
		if flow.Lines[i].Root() != lastLine {
			continue
		}

		flow.Lines = flow.Lines[:i]
		break
	}

	destLine := flow.Lines[0]
	if delta >= 0 {
		destLine = flow.Lines[len(flow.Lines)-1]
	}

	if destLine.Root() == current.Root() {
		return
	}

	destCol := resolveDesiredColumn(destLine.Chars, r.desiredCol)
	r.mode = ModeCopy

	// If the line is on the screen, we don't need to scroll
	viewport := r.Flow(r.viewport, r.root)
	for row, line := range viewport.Lines {
		if line.Root() != destLine.Root() {
			continue
		}

		r.cursor = geom.Vec2{
			R: row,
			C: destCol,
		}
		break
	}

	if delta < 0 {
		r.root = destLine.Root()
		r.cursor = geom.Vec2{
			R: 0,
			C: destCol,
		}
		return
	}

	scrollLines := []emu.ScreenLine{}
	scrollLines = append(scrollLines, viewport.Lines[:r.cursor.R]...)
	scrollLines = append(scrollLines, flow.Lines...)
	r.root = scrollLines[geom.Max(len(scrollLines)-r.viewport.R, 0)].Root()
	r.cursor = geom.Vec2{
		R: r.viewport.R - 1,
		C: destCol,
	}
}
