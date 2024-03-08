package replay

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
)

type flowLine struct {
	// The coordinate of this row
	R int
	// The columns in that row this line occupies, [C0,C1)
	C0, C1 int
	// The slice containing the glyphs in this range
	Chars emu.Line
}

// getFlowLines returns `count` lines from the perspective of `from` respecting
// the bounds of the current viewport. A negative `count` will return -1 *
// `count` lines from before where `from` begins. If there are not enough lines
// before or after `from` to satisfy the request, fewer lines than `count` will
// be returned.
//
// For example, with a history that looks like this:
// ```
// abcdefg
// !abc
// foobarbaz
// ```
// where "!" represents `from`, a `count` of 2 with a viewport width of 3 would
// return:
// ```
// abc
// foo
// ```
//
// A count of -2 and a viewport width of 3 would return:
// ```
// bcd
// efg
// ```
func (r *Replay) getFlowLines(from geom.Vec2, count int) (lines []flowLine, linesOk bool) {
	// We flow the screen right away since we need to do bounds checks
	var (
		history     = r.History()
		numHistory  = len(history)
		isWrapped   = history[numHistory-1].IsWrapped()
		screen      = emu.UnwrapLines(r.Screen())
		numLines    = numHistory + len(screen)
		screenStart = 0
		root        = r.Root()
	)

	// If the last line of history continues onto the screen, we have one less
	// line
	if isWrapped {
		numLines--
		screenStart = 1
	}

	if from.R < 0 || from.R >= numLines {
		return
	}

	getLine := func(index int) (line emu.Line, ok bool) {
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

	return
}

// viewportToFlow returns the point `out` in flow space that corresponds to the
// given point in screen space. If `in` does not correspond to a line in flow
// space, `ok` will be false.
func (r *Replay) viewportToFlow(in geom.Vec2) (out geom.Vec2, ok bool) {
	// TODO(cfoust): 03/08/24 ensure minOffset is zero when in flow mode
	in = r.clampToTerminal(in)

	return
}
