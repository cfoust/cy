package emu

import (
	"github.com/cfoust/cy/pkg/geom"
	"github.com/mattn/go-runewidth"
)

type charRange struct{ y, x0, x1 int }
type physicalLine []charRange

func wrapLine(line Line, cols int) (lines physicalLine) {
	numChars := line.Length()
	// special case: an empty line still returns a flowLine
	if numChars == 0 {
		lines = append(lines, charRange{})
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

		lines = append(lines, charRange{
			x0: i,
			x1: i + len(broken),
		})
		i += len(broken)
	}

	return
}

// Unwrap takes a set of wrapped lines (ie those wrapped to fit a screen) and
// returns unwrapped lines.
func unwrapLines(lines []Line) (unwrapped []physicalLine) {
	var current []charRange
	var line Line
	for row := 0; row < len(lines); row++ {
		line = lines[row]

		current = append(
			current,
			charRange{y: row, x1: line.Length()},
		)

		if line.IsWrapped() && row != len(lines)-1 {
			// Remove attrWrap
			// current[len(current)-1].Mode ^= attrWrap
			continue
		}

		unwrapped = append(unwrapped, current)
		current = make([]charRange, 0)
	}

	return unwrapped
}

func resolveLine(lines []Line, screen physicalLine) (line Line) {
	for _, r := range screen {
		line = append(line, lines[r.y][r.x0:r.x1]...)
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

func wrapCursor(
	oldLines, newLines []Line,
	oldLine, newLine []charRange,
	oldCursor Cursor,
) (newCursor Cursor) {
	// Set styles
	newCursor = oldCursor

	// Find the oldOffset of the old cursor in the old line
	oldOffset := 0
	didFind := false
findOld:
	for row, line := range oldLine {
		numChars := line.x1 - line.x0
		for col := 0; col < numChars; col++ {
			width := runewidth.RuneWidth(
				oldLines[line.y][line.x0+col].Char,
			)

			if oldCursor.Y == row && oldCursor.X >= col && oldCursor.X <= col+width {
				didFind = true
				break findOld
			}

			oldOffset++
			col += width - 1
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
	//
	// This also is a safety mechanism for when the cursor doesn't fall
	// within a cell that contains a printable character
	if !didFind || (oldCursor.State&cursorWrapNext) == 1 {
		oldOffset++
	}

	// Find the cell occupied by the same offset in the new line
	newOffset := 0
findNew:
	for row, line := range newLine {
		numChars := line.x1 - line.x0
		newCursor.Y = row

		for col := 0; col < numChars; col++ {
			newCursor.X = col
			if newOffset == oldOffset {
				break findNew
			}

			width := runewidth.RuneWidth(
				newLines[line.y][line.x0+col].Char,
			)
			newOffset++
			col += width - 1
		}

		// This can only happen if the offset falls on the "next"
		// character after the end of the line, so we need to wrap
		if newOffset == oldOffset {
			newCursor.State |= cursorWrapNext
			break
		}
	}

	return
}

func reflow(oldScreen []Line, oldCursor Cursor, cols int) (newLines []Line, newCursor Cursor, cursorValid bool) {
	// 1. Get the offsets of all of the "physical" lines
	oldWrapped := unwrapLines(oldScreen)

	// 2. Resolve them to full []Lines for calculation purposes
	oldResolved := make([]Line, 0)
	for _, line := range oldWrapped {
		oldResolved = append(
			oldResolved,
			resolveLine(oldScreen, line),
		)
	}

	// Remove trailing empty lines
	for i := len(oldResolved) - 1; i >= 0; i-- {
		line := oldResolved[i]
		if line.Length() != 0 {
			break
		}
		oldResolved = oldResolved[:i]
	}

	// 3. Wrap those full []Lines into new physicalLines
	newWrapped := make([]physicalLine, 0)
	for _, line := range oldResolved {
		newWrapped = append(newWrapped, wrapLine(line, cols))
	}

	// Find the cursor position
	for i := len(oldResolved) - 1; i >= 0; i++ {
		// We want to skip any trailing physical lines we removed
		oldLine := oldWrapped[i]
		if len(oldLine) == 0 {
			continue
		}

		origin := oldLine[0]
		if oldCursor.Y <= origin.y {
			continue
		}

		oldCursor.Y -= origin.y
		newCursor = wrapCursor(
			oldScreen,
			oldResolved,
			oldLine,
			newWrapped[i],
			oldCursor,
		)
	}

	// 4. Turn those physicalLines into a screen
	for _, physical := range newWrapped {
		for i, line := range physical {
			numBlank := cols - (line.x1 - line.x0)
			newLine := make(Line, cols)
			copy(newLine, oldResolved[line.y][line.x0:line.x1])

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
