package replay

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"

	"github.com/mattn/go-runewidth"
)

func (r *Replay) moveCursorX(delta int) {
	if r.isImageMode() {
		r.moveCursorDeltaImage(geom.Vec2{C: delta})
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
		r.moveCursorDeltaImage(geom.Vec2{R: delta})
		return
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

	// Ensure the user can't move past the last physical line
	lastLine := r.getLastFlow()
	for i := 0; i < len(flow.Lines); i++ {
		if flow.Lines[i].Root().R <= lastLine {
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

	r.mode = ModeCopy

	r.cursor.C = resolveDesiredColumn(destLine.Chars, r.desiredCol)

	// If the line is on the screen, we don't need to scroll
	viewport := r.Flow(r.viewport, r.root)
	for row, line := range viewport.Lines {
		if line.Root() != destLine.Root() {
			continue
		}

		r.cursor.R = row
		break
	}

	position := ScrollPositionBottom
	if delta < 0 {
		position = ScrollPositionTop
	}

	r.scrollToFlowLine(destLine.Root(), position)
}
