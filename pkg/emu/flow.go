package emu

import (
	"github.com/cfoust/cy/pkg/geom"
)

type FlowResult struct {
	// The total number of physical lines in the history AND on the screen
	NumLines int
	Lines    []ScreenLine
	OK       bool
	Cursor   Cursor
	CursorOK bool
}

// Coord gets the coordinate for a cell on the "screen" produced by the
// original viewport.
func (f *FlowResult) Coord(pos geom.Vec2) (result geom.Vec2, ok bool) {
	if pos.R < 0 || pos.R >= len(f.Lines) {
		return
	}

	line := f.Lines[pos.R]
	if pos.C < 0 || pos.C >= len(line.Chars) {
		return
	}

	ok = true
	result.R = line.R
	result.C = line.C0 + pos.C
	return
}

func (s *State) accessPhysicalLines() (getLines func(int) (Line, bool), numLines int) {
	var (
		history     = s.history
		numHistory  = len(history)
		isWrapped   = false
		screenLines = unwrapLines(s.screen)
		screen      = resolveLines(s.screen, screenLines)
		screenStart = 0
	)

	numLines = numHistory + len(screen)

	if numHistory > 0 {
		isWrapped = history[numHistory-1].IsWrapped()
	}

	// If the last line of history continues onto the screen, we have one
	// less line
	if isWrapped {
		numLines--
		screenStart = 1
	}

	lastHistory := numHistory - screenStart
	getLines = func(index int) (line Line, ok bool) {
		if index < 0 || index >= numLines {
			return
		}

		ok = true

		if index < lastHistory {
			line = history[index]
			return
		}

		// special case: history line continues onto screen
		if isWrapped && index == lastHistory {
			line = history[len(history)-1].Clone()
			line = append(line, screen[0]...)
			return
		}

		line = screen[index-lastHistory]
		return
	}
	return
}

func (s *State) Flow(
	viewport, root geom.Vec2,
) (result FlowResult) {
	viewport.C = geom.Max(viewport.C, 1)

	var (
		history           = s.history
		numHistory        = len(history)
		isWrapped         = false
		screenLines       = unwrapLines(s.screen)
		cols              = viewport.C
		getLine, numLines = s.accessPhysicalLines()
	)

	if numHistory > 0 {
		isWrapped = history[numHistory-1].IsWrapped()
	}

	result.NumLines = numLines

	if root.C < 0 || root.R < 0 || root.R >= numLines {
		return
	}

	rootLine, rootOk := getLine(root.R)
	if !rootOk || (root.C > 0 && root.C >= len(rootLine)) {
		return
	}

	isBackwards := viewport.R < 0
	viewport.R = geom.Abs(viewport.R)

	var ok bool
	row := root.R
	line := rootLine[root.C:]
	location := ScreenLine{
		R:  row,
		C0: root.C,
	}

	if isBackwards {
		line = rootLine[:root.C]
		location = ScreenLine{
			R: row,
		}

		// If we're going backwards but we're keeping none of the root
		// line, just go up one line.
		if len(line) == 0 {
			row--
			location.R = row
			line, ok = getLine(row)
			if !ok {
				return
			}
		}
	}

	result.OK = true

	for {
		numLeft := geom.Max(viewport.R-len(result.Lines), 0)
		broken := wrapLine(line, cols)

		for i := range broken {
			broken[i].R = location.R
			broken[i].C0 += location.C0
			broken[i].C1 += location.C0
		}

		numBroken := len(broken)

		// We take ALL lines if viewport.R == 0
		if viewport.R == 0 {
			numLeft = numBroken
		}

		if isBackwards {
			result.Lines = append(
				broken[geom.Max(numBroken-numLeft, 0):],
				result.Lines...,
			)
		} else {
			result.Lines = append(
				result.Lines,
				broken[:geom.Min(numBroken, numLeft)]...,
			)
		}

		if len(result.Lines) == viewport.R {
			break
		}

		if isBackwards {
			row--
		} else {
			row++
		}

		location = ScreenLine{R: row}

		line, ok = getLine(row)
		if !ok {
			break
		}
	}

	// Resolve lines
	for i, screenLine := range result.Lines {
		line, ok := getLine(screenLine.R)
		if !ok {
			continue
		}
		result.Lines[i].Chars = line[screenLine.C0:screenLine.C1]
	}

	// translateCursor corrects the cursor by snapping it to the the
	// nearest line
	newCursor := translateCursor(
		s.screen,
		s.screen,
		screenLines,
		screenLines,
		s.Cursor(),
		cols,
	)

	result.Cursor = newCursor.cursor
	cursorLoc := newCursor.location

	// Transform the result into the reference frame of the terminal and
	// its history
	cursorLoc.R += numHistory
	if isWrapped {
		cursorLoc.R--
	}

	for row, screenLine := range result.Lines {
		result.Cursor.R = row

		if cursorLoc.R != screenLine.R || cursorLoc.C < screenLine.C0 || (screenLine.C1 != screenLine.C0 && cursorLoc.C >= screenLine.C1) {
			continue
		}

		result.CursorOK = true
		result.Cursor.C = cursorLoc.C - screenLine.C0

		if newCursor.isEnd {
			result.Cursor.C = geom.Clamp(
				result.Cursor.C+1,
				0,
				viewport.C-1,
			)
		}
		break
	}

	return
}

func (s *State) GetLines(start, end int) (lines []Line) {
	getLine, numLines := s.accessPhysicalLines()

	if end < start {
		start, end = end, start
	}

	start = geom.Clamp(start, 0, numLines)
	end = geom.Clamp(end, 0, numLines)

	for i := start; i < end+1; i++ {
		line, ok := getLine(i)
		if !ok {
			return
		}
		lines = append(lines, line)
	}
	return
}
