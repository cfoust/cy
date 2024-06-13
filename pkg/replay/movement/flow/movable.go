package flow

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
)

func (f *flowMovement) Line(row int) (line emu.Line, ok bool) {
	lines := f.GetLines(row, row)
	if len(lines) == 0 {
		return
	}

	return lines[0], true
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
	root, ok := f.getRoot(location)
	if !ok {
		return
	}

	// TODO(cfoust): 06/05/24 if it's on the screen, don't recenter it
	f.haveMoved = true
	f.scrollToLine(root, ScrollPositionCenter)
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
