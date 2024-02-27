package emu

// Return true if the last line in `lines` continues on to the
// screen (in other words, it's wrapped.)
func hasTrailingWrap(lines []Line) bool {
	if len(lines) == 0 {
		return false
	}

	lastLine := lines[len(lines)-1]
	if len(lastLine) == 0 {
		return false
	}

	return lastLine[len(lastLine)-1].Mode == attrWrap
}

func appendWrapped(lines []Line, line Line) []Line {
	cloned := getOccupiedLine(copyLine(line))

	if !hasTrailingWrap(lines) {
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
		if glyph.Char != ' ' || glyph.FG != DefaultFG || glyph.BG != DefaultBG {
			length = i + 1
			break
		}
	}
	return length
}

func getOccupiedLine(line Line) Line {
	if line[len(line)-1].Mode == attrWrap {
		return line
	}

	return line[:getLineLength(line)]
}

func wrapLine(line Line, cols int) []Line {
	// We only want to wrap non-whitespace characters
	length := getLineLength(line)

	result := make([]Line, 0)

	if length == 0 {
		return []Line{emptyLine(cols)}
	}

	numLines := length / cols
	if (length % cols) > 0 {
		numLines++
	}

	for i := 0; i < numLines; i++ {
		start := i * cols
		end := (i + 1) * cols

		if end <= length {
			result = append(result, line[start:end])
			continue
		}

		// It's the last line, split it up
		newLine := make(Line, cols)
		for j := start; j < end; j++ {
			if j < length {
				newLine[j-start] = line[j]
				continue
			}

			newLine[j-start] = EmptyGlyph()
		}
		result = append(result, newLine)
	}

	// Mark attrWrap
	for i := 0; i < len(result)-1; i++ {
		result[i][cols-1].Mode = attrWrap
	}

	return result
}

type rowRange struct {
	Start int
	End   int // exclusive
}

type lineMapping struct {
	Before rowRange
	After  rowRange
}

func wrapLines(lines []Line, cols int) (newLines []Line, mappings []lineMapping) {
	var current Line = nil
	var start int

	numLines := len(lines)
	var line Line
	for row := 0; row < numLines; row++ {
		line = lines[row]

		// the line was wrapped originally, aggregate it
		wasWrapped := line[len(line)-1].Mode == attrWrap

		if current == nil {
			start = row
			current = copyLine(line)
		} else {
			current = append(current, line...)
		}

		if wasWrapped && row != len(lines)-1 {
			// Remove attrWrap
			current[len(current)-1].Mode ^= attrWrap
			continue
		}

		// We've accumulated the whole line, wrap it
		wrapped := wrapLine(current, cols)
		mappings = append(mappings, lineMapping{
			Before: rowRange{
				Start: start,
				End:   row + 1,
			},
			After: rowRange{
				Start: len(newLines),
				End:   len(newLines) + len(wrapped),
			},
		})

		for _, wrappedLine := range wrapped {
			newLines = append(newLines, wrappedLine)
		}
		current = nil
	}

	return newLines, mappings
}

// reflow recalculates the wrap point for all lines in `screen`.
func reflow(oldLines []Line, oldCursor Cursor, newCols int) (newLines []Line, newCursor Cursor) {
	wrapped, mappings := wrapLines(oldLines, newCols)

	// Remove trailing empty lines
	for i := len(wrapped) - 1; i >= 0; i-- {
		if getLineLength(wrapped[i]) != 0 {
			break
		}
		wrapped = wrapped[:i]
	}

	if len(wrapped) == 0 {
		return wrapped, newCursor
	}

	oldCols := len(oldLines[0])
	curRow := oldCursor.Y
	for _, mapping := range mappings {
		before := mapping.Before
		after := mapping.After
		if curRow < before.Start || curRow >= before.End {
			continue
		}

		newCursor = oldCursor
		offset := (curRow-before.Start)*oldCols + oldCursor.X

		// Logically, if the cursor is about to wrap to the next line,
		// it really means that it's "occupying" the next cell
		if oldCursor.State&cursorWrapNext != 0 {
			offset += 1
		}

		newCursor.X = offset % newCols
		newCursor.Y = after.Start + (offset-newCursor.X)/newCols

		// If the cursor falls on a line boundary, we pretend as though
		// it were about to wrap
		if newCols > 0 && offset > 0 && newCursor.X == 0 {
			newCursor.X = newCols - 1
			newCursor.Y -= 1
			newCursor.State |= cursorWrapNext
		}
		break
	}

	return wrapped, newCursor
}
