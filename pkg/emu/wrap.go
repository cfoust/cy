package emu

import (
	"github.com/cfoust/cy/pkg/geom"
	"github.com/mattn/go-runewidth"
)

type ScreenLine struct {
	// The coordinate of this row
	R int
	// The columns in that row this line occupies, [C0,C1)
	C0, C1 int
	// The slice containing the glyphs in this range
	Chars Line
}

// Root returns the coordinate of the beginning of this ScreenLine.
func (l ScreenLine) Root() geom.Vec2 {
	return geom.Vec2{
		R: l.R,
		C: l.C0,
	}
}

type physicalLine []ScreenLine

func wrapLine(line Line, cols int) (lines physicalLine) {
	numChars := line.Length()
	// special case: an empty line still returns a flowLine
	if numChars == 0 {
		lines = append(lines, ScreenLine{})
		return
	}

	// if cursor is on or directly after a non-attrBlank cell, flow it
	// normally
	// if cursor is on a blank line, just do .X % cols
	// if cursor is after end of line with non-attrBlank characters, move
	// back to cell after last
	// if cursor is after last line, move back to the first blank line

	i := 0
	for i < numChars {
		broken := line[i:geom.Min(i+cols, numChars)]

		// This can only happen if the final character is a double wide
		// glyph
		if len(broken) > 0 && broken.Length() > len(broken) {
			broken = broken[:len(broken)-1]
		}

		if len(broken) == 0 {
			return
		}

		lines = append(lines, ScreenLine{
			C0: i,
			C1: i + len(broken),
		})
		i += len(broken)
	}

	return
}

// Unwrap takes a set of wrapped lines (ie those wrapped to fit a screen) and
// returns unwrapped lines.
func unwrapLines(lines []Line) (unwrapped []physicalLine) {
	var current []ScreenLine
	var line Line
	for row := 0; row < len(lines); row++ {
		line = lines[row]

		current = append(
			current,
			ScreenLine{R: row, C1: line.Length()},
		)

		if line.IsWrapped() && row != len(lines)-1 {
			continue
		}

		unwrapped = append(unwrapped, current)
		current = make([]ScreenLine, 0)
	}

	return unwrapped
}

func resolveLine(lines []Line, screen physicalLine) (line Line) {
	for _, r := range screen {
		line = append(line, lines[r.R][r.C0:r.C1]...)
	}

	// Remove the wrap state
	for i := range line {
		line[i].Mode &= ^attrWrap
	}

	return
}

// isWrappedLines returns true if the last line in `lines` is wrapped.
func isWrappedLines(lines []Line) bool {
	if len(lines) == 0 {
		return false
	}

	return lines[len(lines)-1].IsWrapped()
}

func appendWrapped(lines []Line, line Line) []Line {
	cloned := getOccupiedLine(copyLine(line))

	if !isWrappedLines(lines) {
		return append(lines, cloned)
	}

	lastIndex := len(lines) - 1
	lines[lastIndex] = append(lines[lastIndex], cloned...)
	return lines
}

func emptyLine(cols int) Line {
	line := make(Line, cols)
	for i := range line {
		line[i] = EmptyGlyph()
	}
	return line
}

func getLineLength(line Line) int {
	var length int = 0
	for i := len(line) - 1; i >= 0; i-- {
		glyph := line[i]
		if glyph.IsDefault() {
			continue
		}

		length = i + runewidth.RuneWidth(glyph.Char)
		break
	}
	return length
}

func getOccupiedLine(line Line) Line {
	if line[len(line)-1].Mode == attrWrap {
		return line
	}

	return line[:getLineLength(line)]
}

type wrappedCursor struct {
	cursor   Cursor
	location geom.Vec2
	isEnd    bool
}

func wrapCursor(
	oldLines, newLines []Line,
	oldLine, newLine physicalLine,
	oldCursor Cursor,
	numCols int,
) (newCursor wrappedCursor) {
	// Set styles
	newCursor.cursor = oldCursor

	// Find the oldOffset of the old cursor in the old line
	oldOffset := 0
	didFind := false
findOld:
	for row, line := range oldLine {
		numChars := line.C1 - line.C0
		for col := 0; col < numChars; col++ {
			width := runewidth.RuneWidth(
				oldLines[line.R][line.C0+col].Char,
			)

			if oldCursor.R == row && oldCursor.C >= col && oldCursor.C < col+width {
				didFind = true
				break findOld
			}

			oldOffset++
			col += width - 1
		}

		// a safety mechanism for when the cursor doesn't fall within a
		// cell that contains a printable character
		if oldCursor.R == row && !didFind {
			break
		}
	}

	// If lines are full, but cursor was beyond, we need to set wrap
	// e.g:
	// xxxx|
	// xxxx|
	//   c
	// becomes
	// xxxx|
	// xxxx|c
	if (oldCursor.State & cursorWrapNext) != 0 {
		oldOffset++
	}

	// Find the cell occupied by the same offset in the new line
	newOffset := 0
findNew:
	for row, line := range newLine {
		numChars := line.C1 - line.C0
		newCursor.location.R = line.R
		newCursor.cursor.R = row
		isLast := false

		for col := 0; col < numChars; col++ {
			newCursor.location.C = line.C0 + col
			newCursor.cursor.C = col

			// The cursor is within the line (not at the end)
			if newOffset == oldOffset {
				break findNew
			}

			width := runewidth.RuneWidth(
				newLines[line.R][line.C0+col].Char,
			)
			isLast = (col + width) == numCols
			newOffset++
			col += width - 1
		}

		// This can only happen if the offset falls on the "next"
		// character after the end of the line, so we need to wrap
		if row == len(newLine)-1 && newOffset == oldOffset {
			newCursor.isEnd = numChars > 0
			if isLast {
				newCursor.cursor.State |= cursorWrapNext
			} else if numChars > 0 {
				newCursor.cursor.C++
			}
			break
		}
	}

	return
}

func translateCursor(
	oldLines, newLines []Line,
	oldPhysical, newPhysical []physicalLine,
	oldCursor Cursor,
	cols int,
) (newCursor wrappedCursor) {
	for i := len(newPhysical) - 1; i >= 0; i-- {
		// We want to skip any trailing physical lines we removed
		oldLine := oldPhysical[i]
		if len(oldLine) == 0 {
			continue
		}

		origin := oldLine[0]
		if oldCursor.R < origin.R {
			continue
		}

		oldCursor.R -= origin.R
		newCursor = wrapCursor(
			oldLines,
			newLines,
			oldLine,
			newPhysical[i],
			oldCursor,
			cols,
		)

		// Make cursor row relative to new lines
		for j := 0; j < i; j++ {
			newCursor.cursor.R += len(newPhysical[j])
		}

		break
	}

	return
}

func resolveLines(lines []Line, physical []physicalLine) (resolved []Line) {
	for _, line := range physical {
		resolved = append(resolved, resolveLine(lines, line))
	}

	return
}

func wrapLines(lines []Line, cols int) (wrapped []physicalLine) {
	for row, line := range lines {
		wrappedLine := wrapLine(line, cols)
		for i := range wrappedLine {
			wrappedLine[i].R = row
		}
		wrapped = append(wrapped, wrappedLine)
	}
	return
}

func reflow(oldScreen []Line, oldCursor Cursor, cols int) (newLines []Line, newCursor Cursor, cursorValid bool) {
	// 1. Get the offsets of all of the "physical" lines
	oldWrapped := unwrapLines(oldScreen)

	// 2. Resolve them to full []Lines for calculation purposes
	oldResolved := resolveLines(oldScreen, oldWrapped)

	// Remove trailing empty lines
	for i := len(oldResolved) - 1; i >= 0; i-- {
		line := oldResolved[i]
		if line.Length() != 0 {
			break
		}
		oldResolved = oldResolved[:i]
	}

	cursorValid = len(oldResolved) > 0

	// 3. Wrap those full []Lines into new physicalLines
	newWrapped := wrapLines(oldResolved, cols)

	// Find the cursor position
	newCursor = translateCursor(
		oldScreen,
		oldResolved,
		oldWrapped,
		newWrapped,
		oldCursor,
		cols,
	).cursor

	// 4. Turn those physicalLines into a screen
	for _, physical := range newWrapped {
		for i, line := range physical {
			numBlank := cols - (line.C1 - line.C0)
			newLine := make(Line, cols)
			copy(newLine, oldResolved[line.R][line.C0:line.C1])

			// Fill in any blank cells
			for j := cols - numBlank; j < cols; j++ {
				newLine[j] = EmptyGlyph()
				newLine[j].Mode |= attrBlank
			}

			// Mark wrapped
			if i != len(physical)-1 {
				newLine[cols-1].Mode ^= attrWrap
			}

			newLines = append(newLines, newLine)
		}
	}

	return
}
