package flow

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
)

func resolveDesiredColumn(line emu.Line, col int) int {
	occupancy := line.Occupancy()
	if col >= len(occupancy) {
		return geom.Max(len(occupancy)-1, 0)
	}

	// desiredCol occupied -> return that col
	if occupancy[col] {
		return col
	}

	var haveBefore, haveAfter bool
	// check for occupied cells before and after the desired column
	for i := 0; i < len(line); i++ {
		if i == col || !occupancy[i] {
			continue
		}

		if i > col {
			haveAfter = true
		} else {
			haveBefore = true
		}
	}

	// the line is empty, just go to col 0
	if !haveBefore && !haveAfter {
		return 0
	}

	// col is before last non-whitespace and after first
	// non-whitespace: remain in place
	if haveBefore && haveAfter {
		return col
	}

	// first non-whitespace is after col: last column before first
	// non-whitespace
	if haveAfter && !haveBefore {
		first, _ := line.Whitespace()
		return geom.Max(first-1, 0)
	}

	// last non-whitespace is before col: last non-whitespace column
	if haveBefore && !haveAfter {
		_, last := line.Whitespace()
		return last
	}

	return 0
}
