package replay

import (
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
)

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
	r.setImageOffsetY(-1)

	if r.isCopyMode() {
		r.centerPoint(r.cursor)
	} else {
		r.centerPoint(r.getTerminalCursor())
	}

	return r, nil
}

// scrollYDelta moves the viewport up and down, "dragging" the cursor along behind it.
func (r *Replay) scrollYDelta(delta int) {
	if delta == 0 {
		return
	}

	r.isPlaying = false
	r.mode = ModeCopy

	if r.isImageMode() {
		before := r.viewportToTerm(r.cursor)
		r.setImageOffsetY(r.offset.R + delta)
		r.cursor = r.termToViewport(before).Clamp(
			geom.Vec2{},
			geom.Vec2{
				R: r.viewport.R - 1,
				C: r.viewport.C - 1,
			},
		)
		return
	}

	isUp := delta < 0

	// Account for the fact that Flow() returns the root line as well
	if !isUp {
		delta++
	}

	result := r.Flow(geom.Vec2{
		C: r.viewport.C,
		R: delta,
	}, r.root)

	numLines := len(result.Lines)

	// Find the new root
	target := 0
	if !isUp {
		target = numLines - 1
	}

	targetLine := result.Lines[target]
	r.root = geom.Vec2{
		R: targetLine.R,
		C: targetLine.C0,
	}

	newRow := r.cursor.R
	if isUp {
		newRow += numLines
	} else {
		// -1 because we're skipping the root line
		newRow -= numLines - 1
	}

	newRow = geom.Clamp(newRow, 0, r.viewport.R-1)
	r.cursor = geom.Vec2{
		R: newRow,
		C: r.resolveScreenColumn(newRow),
	}
}

func (r *Replay) setScrollX(offset int) {
	r.isPlaying = false
	r.mode = ModeCopy
	before := r.viewportToTerm(r.cursor)
	r.setImageOffsetX(offset)
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
