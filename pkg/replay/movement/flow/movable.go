package flow

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
)

func (f *flowMovement) Line(row int) (line emu.Line, ok bool) {
	if row < 0 {
		return
	}

	lines := f.GetLines(row, row)
	if len(lines) == 0 {
		return
	}

	return lines[0], true
}

func (f *flowMovement) NumLines() int {
	lastRoot := f.getLastRoot()
	return lastRoot.R + 1
}

func (f *flowMovement) Cursor() geom.Vec2 {
	result := f.Flow(f.viewport, f.root)

	for row, line := range result.Lines {
		if f.cursor.R != row {
			continue
		}

		numChars := line.C1 - line.C0
		cursor := geom.Vec2{
			R: line.R,
			C: line.C0 + f.cursor.C,
		}

		// We need to return the address of a real cell
		if f.cursor.C >= numChars {
			_, lastCell := line.Chars.Whitespace()
			cursor.C = lastCell
		}

		return cursor
	}

	return geom.Vec2{}
}

func (f *flowMovement) Goto(location geom.Vec2) {
	// First check whether the location is on the screen already
	flow := f.Flow(f.viewport, f.root)
	for row, line := range flow.Lines {
		if location.R != line.R || location.C < line.C0 || location.C >= line.C1 {
			continue
		}

		f.haveMoved = true
		f.cursor.R = row
		f.cursor.C = location.C - line.C0
		f.desiredCol = f.cursor.C
		return
	}

	root, ok := f.getRoot(location)
	if !ok {
		return
	}

	f.haveMoved = true

	// Move the screen as little as possible
	position := ScrollPositionTop
	if root.GT(f.root) {
		position = ScrollPositionBottom
	}
	f.scrollToLine(root, position)

	f.cursor.C = location.C - root.C
	f.desiredCol = f.cursor.C
}

func (f *flowMovement) Viewport() (
	lines []emu.ScreenLine,
	size geom.Vec2,
	cursor geom.Vec2,
) {
	result := f.Flow(f.viewport, f.root)
	return result.Lines, f.viewport, f.cursor
}
