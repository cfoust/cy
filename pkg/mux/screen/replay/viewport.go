package replay

import (
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
)

// Ensure a point in term space falls inside of the terminal and its scrollback.
func (r *Replay) clampToTerminal(point geom.Vec2) geom.Vec2 {
	return point.Clamp(
		r.minOffset,
		r.getTerminalSize().Sub(geom.UnitVec2),
	)
}

// Translate a coordinate in the reference frame of the terminal to a point in
// the viewport.
func (r *Replay) termToViewport(point geom.Vec2) geom.Vec2 {
	return point.Sub(r.offset)
}

func (r *Replay) viewportToTerm(point geom.Vec2) geom.Vec2 {
	return point.Add(r.offset)
}

// Check whether the given point in viewport space actually falls within it.
func (r *Replay) isInViewport(point geom.Vec2) bool {
	if point.R < 0 || point.C < 0 || point.R >= r.viewport.R || point.C >= r.viewport.C {
		return false
	}

	return true
}

func (r *Replay) setViewport(oldViewport, newViewport geom.Size) (taro.Model, tea.Cmd) {
	// Remove one row for our status line
	newViewport.R = geom.Max(newViewport.R-1, 0)
	r.viewport = newViewport
	r.recalculateViewport()
	r.setOffsetY(-1)

	if r.isCopyMode() {
		r.centerPoint(r.cursor)
	} else {
		r.centerPoint(r.getTerminalCursor())
	}

	return r, nil
}

func (r *Replay) setOffsetY(offset int) {
	cursor := r.viewportToTerm(r.cursor)
	desiredCol := r.desiredCol + r.offset.C
	r.offset.R = geom.Clamp(offset, r.minOffset.R, r.maxOffset.R)
	r.cursor = r.termToViewport(cursor)
	r.desiredCol = desiredCol - r.offset.C
}

func (r *Replay) setOffsetX(offset int) {
	cursor := r.viewportToTerm(r.cursor)
	desiredCol := r.desiredCol + r.offset.C
	r.offset.C = geom.Clamp(offset, r.minOffset.C, r.maxOffset.C)
	r.cursor = r.termToViewport(cursor)
	r.desiredCol = desiredCol - r.offset.C
}

// Center the viewport on a point in the reference frame of the terminal.
func (r *Replay) centerPoint(point geom.Vec2) {
	r.setOffsetX(point.C - (r.viewport.C / 2))
	r.setOffsetY(point.R - (r.viewport.R / 2))
}

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
	r.setOffsetY(r.offset.R)
	r.setOffsetX(r.offset.C)
}

func (r *Replay) setScrollY(offset int) {
	r.isPlaying = false
	r.mode = ModeCopy
	before := r.viewportToTerm(r.cursor)
	r.setOffsetY(offset)
	after := r.termToViewport(before)

	// cursor is below viewport; move it to bottom
	if after.R >= r.viewport.R {
		r.cursor.R = geom.Max(r.viewport.R-1, 0)
	} else if after.R < 0 {
		r.cursor.R = 0
	} else {
		r.cursor.R = after.R
	}

	r.cursor.C = r.resolveDesiredColumn(geom.Vec2{
		R: r.cursor.R + r.offset.R,
		C: r.desiredCol,
	})
}

func (r *Replay) setScrollX(offset int) {
	r.isPlaying = false
	r.mode = ModeCopy
	before := r.viewportToTerm(r.cursor)
	r.setOffsetX(offset)
	after := r.termToViewport(before)

	if after.C >= r.viewport.C {
		r.cursor.C = geom.Max(r.viewport.C-1, 0)
	} else if after.C < 0 {
		r.cursor.C = 0
	} else {
		r.cursor.C = after.C
	}

	r.desiredCol = after.C
}
