package movement

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"

	"github.com/mattn/go-runewidth"
)

// Get the occupancy state of the given line.
func getOccupancy(line emu.Line) []bool {
	occupancy := make([]bool, len(line))
	for i := 0; i < len(line); i++ {
		if line[i].IsEmpty() {
			continue
		}

		// handle wide runes
		r := line[i].Char
		w := runewidth.RuneWidth(r)
		for j := 0; j < w; j++ {
			occupancy[i+j] = true
		}
		i += geom.Max(w-1, 0)
	}

	return occupancy
}

func isLineEmpty(line emu.Line) bool {
	occupancy := getOccupancy(line)

	for _, occupied := range occupancy {
		if occupied {
			return false
		}
	}

	return true
}

// Get the indices of the first and last non-empty cells for the given line.
func getNonWhitespace(line emu.Line) (first, last int) {
	for i := 0; i < len(line); i++ {
		if line[i].IsEmpty() {
			continue
		}

		first = i
		break
	}

	for i := len(line) - 1; i >= 0; i-- {
		if line[i].IsEmpty() {
			continue
		}

		last = i
		break
	}

	return
}

func resolveDesiredColumn(line emu.Line, col int) int {
	occupancy := getOccupancy(line)
	if col >= len(occupancy) {
		return len(occupancy) - 1
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
		first, _ := getNonWhitespace(line)
		return geom.Max(first-1, 0)
	}

	// last non-whitespace is before col: last non-whitespace column
	if haveBefore && !haveAfter {
		_, last := getNonWhitespace(line)
		return last
	}

	return 0
}
