package emu

import (
	"github.com/cfoust/cy/pkg/geom"
)

type FlowLine struct {
	// The coordinate of this row
	R int
	// The columns in that row this line occupies, [C0,C1)
	C0, C1 int
	// The slice containing the glyphs in this range
	Chars Line
}

// breakLine turns a single line into `flowLine`s by wrapping to fit into
// `cols`.
func breakLine(line Line, R, C0, cols int) (lines []FlowLine) {
	numChars := line.Length()
	// special case: an empty line still returns a flowLine
	if numChars == 0 {
		lines = append(lines, FlowLine{
			R: R,
		})
		return
	}

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

		lines = append(lines, FlowLine{
			R:     R,
			C0:    C0 + i,
			C1:    C0 + i + len(broken),
			Chars: broken,
		})
		i += len(broken)
	}

	return
}

func (s *State) Flow(
	viewport, root geom.Vec2,
	count int,
) (lines []FlowLine, linesOk bool) {
	// We flow the screen right away since we need to do bounds checks
	var (
		history     = s.history
		numHistory  = len(history)
		isWrapped   = false
		screen      = UnwrapLines(s.screen)
		numLines    = numHistory + len(screen)
		cols        = viewport.C
		screenStart = 0
	)

	if numHistory > 0 {
		isWrapped = history[numHistory-1].IsWrapped()
	}

	// If the last line of history continues onto the screen, we have one less
	// line
	if isWrapped {
		numLines--
		screenStart = 1
	}

	if count == 0 || root.C < 0 || root.R < 0 || root.R >= numLines {
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
	isBackwards := count < 0
	count = geom.Abs(count)

	var ok bool
	row := root.R
	c0 := root.C
	line := rootLine[root.C:]

	if isBackwards {
		c0 = 0
		line = rootLine[:root.C]

		if len(line) == 0 {
			row--
			line, ok = getLine(row)
		}
	}

	for {
		numLeft := geom.Max(count-len(lines), 0)
		broken := breakLine(
			line,
			row,
			c0,
			cols,
		)

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

		if len(lines) == count {
			return
		}

		c0 = 0
		if isBackwards {
			row--
		} else {
			row++
		}

		line, ok = getLine(row)
		if !ok {
			return
		}
	}
}
