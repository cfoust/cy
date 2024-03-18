package emu

import (
	"github.com/cfoust/cy/pkg/geom"
)

type FlowResult struct {
	Lines    []ScreenLine
	OK       bool
	Cursor   Cursor
	CursorOK bool
}

func (s *State) Flow(
	viewport, root geom.Vec2,
) (result FlowResult) {
	viewport.C = geom.Max(viewport.C, 1)

	// We flow the screen right away since we need to do bounds checks
	var (
		history     = s.history
		numHistory  = len(history)
		isWrapped   = false
		screenLines = unwrapLines(s.screen)
		screen      = resolveLines(s.screen, screenLines)
		numLines    = numHistory + len(screen)
		cols        = viewport.C
		screenStart = 0
	)

	if numHistory > 0 {
		isWrapped = history[numHistory-1].IsWrapped()
	}

	// If the last line of history continues onto the screen, we have one
	// less line
	if isWrapped {
		numLines--
		screenStart = 1
	}

	if viewport.R == 0 || root.C < 0 || root.R < 0 || root.R >= numLines {
		return
	}

	getLine := func(index int) (line Line, ok bool) {
		if index < 0 || index >= numLines {
			return
		}

		ok = true
		if index < numHistory-1 {
			line = history[index]
			return
		}

		// special case: history line continues onto screen
		if index == numHistory-1 {
			line = history[len(history)-1].Clone()
			line = append(line, screen[0]...)
			return
		}

		line = screen[(index-numHistory)+screenStart]
		return
	}

	rootLine, rootOk := getLine(root.R)
	if !rootOk || root.C >= len(rootLine) {
		return
	}

	result.OK = true
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

		if len(line) == 0 {
			row--
			line, ok = getLine(row)
		}
	}

	for {
		numLeft := geom.Max(viewport.R-len(result.Lines), 0)
		broken := wrapLine(line, cols)

		for i := range broken {
			broken[i].R = location.R
			broken[i].C0 += location.C0
			broken[i].C1 += location.C0
		}

		numBroken := len(broken)
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

	// TODO(cfoust): 03/18/24 cursor

	return
}
