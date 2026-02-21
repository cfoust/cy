package flow

import (
	"math"
	"strings"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/replay/movement"
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
		if location.R != line.R || location.C < line.C0 ||
			location.C >= line.C1 {
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

func (f *flowMovement) readStringChar(
	start, end geom.Vec2,
	lines []emu.Line,
) string {
	if start.R == end.R {
		line := lines[0]
		if end.C+1 >= len(line) {
			return line[start.C:].String()
		}
		return line[start.C : end.C+1].String()
	}

	var result string
	result += lines[0][start.C:].String() + "\n"

	for i := 1; i < len(lines)-1; i++ {
		result += lines[i].String() + "\n"
	}

	last := lines[len(lines)-1]
	if end.C+1 >= len(last) {
		result += last.String()
	} else {
		result += last[:end.C+1].String()
	}

	return result
}

func (f *flowMovement) readStringBlock(
	start, end geom.Vec2,
	lines []emu.Line,
) string {
	var (
		minC  = geom.Min(start.C, end.C)
		maxC  = geom.Max(start.C, end.C)
		parts []string
	)
	for _, line := range lines {
		if len(line) == 0 {
			parts = append(parts, "")
			continue
		}
		lineMinC := geom.Clamp(minC, 0, len(line)-1)
		lineMaxC := geom.Min(maxC+1, len(line))
		if lineMinC >= lineMaxC {
			parts = append(parts, "")
		} else {
			parts = append(
				parts,
				line[lineMinC:lineMaxC].String(),
			)
		}
	}
	return strings.Join(parts, "\n")
}

func (f *flowMovement) readStringCircle(
	start, end geom.Vec2,
) string {
	flow := f.Flow(f.viewport, f.root)
	if !flow.OK {
		return ""
	}

	fromScreen, fromOK := movementToScreen(
		flow.Lines, start,
	)
	toScreen, toOK := movementToScreen(
		flow.Lines, end,
	)
	if !fromOK || !toOK {
		return ""
	}

	type spanInfo struct {
		row, left, right int
	}
	var spans []spanInfo
	minLeft := math.MaxInt32

	geom.FilledCircleSpans(
		fromScreen, toScreen,
		func(row, left, right int) {
			if row < 0 ||
				row >= len(flow.Lines) {
				return
			}
			line := flow.Lines[row]
			lineLen := len(line.Chars)
			left = geom.Max(left, 0)
			right = geom.Min(right, lineLen-1)
			spans = append(spans, spanInfo{
				row, left, right,
			})
			if left < minLeft {
				minLeft = left
			}
		},
	)

	var parts []string
	for _, s := range spans {
		pad := strings.Repeat(" ", s.left-minLeft)
		if s.left > s.right {
			parts = append(parts, pad)
			continue
		}
		chars := flow.Lines[s.row].Chars
		parts = append(
			parts,
			pad+chars[s.left:s.right+1].String(),
		)
	}
	return strings.Join(parts, "\n")
}

func (f *flowMovement) ReadString(
	start, end geom.Vec2,
	mode movement.SelectionMode,
) (result string) {
	start, end = geom.NormalizeRange(start, end)

	switch mode {
	case movement.SelectCircle:
		return f.readStringCircle(start, end)
	case movement.SelectLine:
		start.C = 0
		end.C = math.MaxInt32
	}

	numLines := end.R - start.R + 1
	lines := f.GetLines(start.R, end.R)
	if len(lines) != numLines {
		return
	}

	switch mode {
	case movement.SelectBlock:
		return f.readStringBlock(start, end, lines)
	default:
		return f.readStringChar(start, end, lines)
	}
}

func (f *flowMovement) ViewportToMovement(coord geom.Vec2) geom.Vec2 {
	flow := f.Flow(f.viewport, f.root)

	if !flow.OK || coord.R < 0 || coord.R >= len(flow.Lines) {
		return geom.Vec2{}
	}

	line := flow.Lines[coord.R]

	movementC := line.C0 + coord.C

	if line.C1 > line.C0 {
		movementC = geom.Clamp(movementC, line.C0, line.C1-1)
	} else {
		movementC = line.C0
	}

	return geom.Vec2{
		R: line.R,
		C: movementC,
	}
}
