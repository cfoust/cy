package flow

import (
	"github.com/cfoust/cy/pkg/geom"
)

type ScrollPosition int

const (
	ScrollPositionTop ScrollPosition = iota
	ScrollPositionCenter
	ScrollPositionBottom
)

func (f *flowMovement) ScrollTop() {
	f.haveMoved = true
	f.scrollToLine(geom.Vec2{R: f.getFirstLine(), C: 0}, ScrollPositionTop)
	f.cursor.C = f.resolveScreenColumn(f.cursor.R)
}

func (f *flowMovement) ScrollBottom() {
	f.haveMoved = true
	f.scrollToLine(f.getLastRoot(), ScrollPositionBottom)
	f.cursor.C = f.resolveScreenColumn(f.cursor.R)
}

func (f *flowMovement) ScrollYDelta(delta int) {
	f.haveMoved = true

	isUp := delta < 0

	// Account for the fact that Flow() returns the root line as well
	if !isUp {
		delta++
	}

	result := f.Flow(geom.Vec2{
		C: f.viewport.C,
		R: delta,
	}, f.root)

	numLines := len(result.Lines)

	if numLines == 0 {
		return
	}

	// Find the new root
	target := 0
	if !isUp {
		lastLine := f.getLastLine()

		// Ensure that we can't scroll past the last physical line
		for i := numLines - 1; i >= 0; i-- {
			target = i
			if result.Lines[i].Root().R <= lastLine {
				break
			}
		}
	}

	targetLine := result.Lines[target]
	f.root = targetLine.Root()

	newRow := f.cursor.R
	if isUp {
		newRow += numLines
	} else {
		// -1 because we're skipping the root line
		newRow -= numLines - 1
	}

	newRow = geom.Clamp(newRow, 0, f.viewport.R-1)
	f.cursor = geom.Vec2{
		R: newRow,
		C: f.resolveScreenColumn(newRow),
	}
}

func (f *flowMovement) ScrollXDelta(delta int) {
	// no-op in this mode
}

func (f *flowMovement) getFirstLine() int {
	// When history is pruned, the coordinate system remains global but we can
	// only access the lines still retained in memory. Find the first accessible
	// physical line via binary search.
	low := 0
	high := geom.Max(f.Root().R, 0)

	for low < high {
		mid := low + (high-low)/2
		if len(f.GetLines(mid, mid)) > 0 {
			high = mid
		} else {
			low = mid + 1
		}
	}

	return low
}

func (f *flowMovement) scrollToLine(dest geom.Vec2, position ScrollPosition) {
	if dest.R < 0 || dest.C < 0 {
		return
	}

	firstLine := f.getFirstLine()
	if dest.R < firstLine {
		dest = geom.Vec2{R: firstLine, C: 0}
	}

	if dest.R > f.getLastLine() {
		return
	}

	// If the line is on the screen, we don't need to scroll
	viewport := f.Flow(f.viewport, f.root)
	for row, line := range viewport.Lines {
		if line.Root() != dest {
			continue
		}

		f.cursor.R = row
		break
	}

	var rows int
	switch position {
	case ScrollPositionCenter:
		rows = f.viewport.R / 2
	case ScrollPositionBottom:
		rows = f.viewport.R - 1
	}

	rows = geom.Max(rows, 0)

	// Skip the relative calculation if we don't need to (or cannot) set
	// the root any higher
	if rows == 0 || (rows > 0 && dest == geom.Vec2{}) {
		f.root = dest
		f.cursor.R = 0
		return
	}

	flow := f.Flow(geom.Vec2{
		C: f.viewport.C,
		R: -1 * rows,
	}, dest)
	if !flow.OK {
		return
	}

	lines := flow.Lines
	f.root = lines[0].Root()
	f.cursor.R = len(lines)
}
