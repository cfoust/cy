package replay

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
)

// Calculate the bounds of `{min,max}Offset` and ensure `offset` falls between them.
func (r *Replay) recalculateViewport() {
	termSize := r.getTerminalSize()
	r.minOffset = geom.Vec2{
		R: -len(r.History()),
		C: 0, // always, but for clarity
	}
	r.maxOffset = geom.Vec2{
		R: geom.Max(termSize.R-r.viewport.R, 0),
		C: geom.Max(termSize.C-r.viewport.C, 0),
	}
	r.setImageOffsetY(r.offset.R)
	r.setImageOffsetX(r.offset.C)
}

// Ensure a point in term space falls inside of the terminal and its scrollback.
func (r *Replay) clampToTerminal(point geom.Vec2) geom.Vec2 {
	return point.Clamp(
		r.minOffset,
		r.getTerminalSize().Sub(geom.UnitVec2),
	)
}

// Center the viewport on a point in the reference frame of the terminal.
func (r *Replay) centerPoint(point geom.Vec2) {
	r.setImageOffsetX(point.C - (r.viewport.C / 2))
	r.setImageOffsetY(point.R - (r.viewport.R / 2))
}

// Translate a coordinate in the reference frame of the terminal to a point in
// the viewport.
func (r *Replay) termToViewport(point geom.Vec2) geom.Vec2 {
	return point.Sub(r.offset)
}

func (r *Replay) viewportToTerm(point geom.Vec2) geom.Vec2 {
	return point.Add(r.offset)
}

func (r *Replay) setImageOffset(offset geom.Vec2) {
	cursor := r.viewportToTerm(r.cursor)
	desiredCol := r.desiredCol + r.offset.C
	r.offset = r.offset.
		Add(offset).
		Clamp(r.minOffset, r.maxOffset)
	r.cursor = r.termToViewport(cursor)
	r.desiredCol = desiredCol - r.offset.C
}

func (r *Replay) setImageOffsetY(offset int) {
	r.setImageOffset(geom.Vec2{R: offset})
}

func (r *Replay) setImageOffsetX(offset int) {
	r.setImageOffset(geom.Vec2{C: offset})
}

// Get the glyphs for a row in term space.
func (r *Replay) getImageLine(row int) emu.Line {
	screen := r.Screen()
	history := r.History()

	// Handle out-of-bounds lines
	clamped := geom.Clamp(row, -len(history), r.getTerminalSize().R-1)
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
func (r *Replay) moveCursorImage(point geom.Vec2) {
	viewport := r.viewport
	newCursor := r.termToViewport(point)

	if newCursor.C < 0 {
		r.setImageOffsetX(r.offset.C + newCursor.C)
	}

	if newCursor.C >= viewport.C {
		r.setImageOffsetX(r.offset.C + (newCursor.C - viewport.C + 1))
	}

	if newCursor.R < 0 {
		r.setImageOffsetY(r.offset.R + newCursor.R)
	}

	if newCursor.R >= viewport.R {
		r.setImageOffsetY(r.offset.R + (newCursor.R - viewport.R + 1))
	}

	r.cursor = newCursor
}

func (r *Replay) moveCursorDeltaImage(delta geom.Vec2) {
	r.mode = ModeCopy
	oldPos := r.viewportToTerm(r.cursor)
	newPos := r.clampToTerminal(oldPos.Add(delta))
	r.moveCursorImage(newPos)
}
