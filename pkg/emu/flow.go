package emu

import (
	"github.com/cfoust/cy/pkg/geom"
)

func (s *State) Flow(
	viewport, root geom.Vec2,
) (
	lines []ScreenLine,
	cursor geom.Vec2,
	linesOk bool,
) {
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

	linesOk = true
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
		numLeft := geom.Max(viewport.R-len(lines), 0)
		broken := wrapLine(line, cols)

		for i := range broken {
			broken[i].R = location.R
			broken[i].C0 += location.C0
			broken[i].C1 += location.C0
		}

		numBroken := len(broken)
		if isBackwards {
			lines = append(
				broken[geom.Max(numBroken-numLeft, 0):],
				lines...,
			)
		} else {
			lines = append(
				lines,
				broken[:geom.Min(numBroken, numLeft)]...,
			)
		}

		if len(lines) == viewport.R {
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
	for i, screenLine := range lines {
		line, ok := getLine(screenLine.R)
		if !ok {
			continue
		}
		lines[i].Chars = line[screenLine.C0:screenLine.C1]
	}

	// TODO(cfoust): 03/18/24 cursor

	return
}
