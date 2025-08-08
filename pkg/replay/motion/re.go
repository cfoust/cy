package motion

import (
	"regexp"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/re"
)

// getBackwardsMatch finds the closest match on `line` behind `from`.
func getBackwardsMatch(
	from geom.Vec2,
	line emu.Line,
	row int,
	matches [][]int,
) (to emu.ScreenLine, ok bool) {
	if len(matches) == 0 {
		return
	}

	for i := len(matches) - 1; i >= 0; i-- {
		match := matches[i]

		if from.LTE(geom.Vec2{
			R: row,
			C: match[0],
		}) {
			continue
		}

		to.R = row
		to.C0 = match[0]
		to.C1 = match[1]
		to.Chars = line[match[0]:match[1]]
		ok = true
		return
	}

	return
}

// findNext gets the next match of a regex pattern in the given direction.
//
// The origin of the search, `from`, is a coordinate in the reference frame of
// the Movable and determines where the search will start from. `from.C`, or
// the initial column of the search, has some special behavior in order to
// mimic the way vim's incremental search works.
//
// When searching forwards, the cell specified by `from.C` will be _excluded_
// from the search. This is to prevent the pattern from matching the current
// location. In addition, specifically when searching forwards, `from.C = -1`
// is considered to be valid; this means that no cell will be excluded from the
// search, which will start from the beginning of the line.
//
// When searching backwards, matches must begin before the cell specified by
// `from`, but can end after or including `from`. Observe that vim works this
// way. Similarly, if `from.C` is equal to the length of the line, no cell will
// be excluded in the search.
func findNext(
	m Movable,
	pattern *regexp.Regexp,
	from geom.Vec2,
	isForward bool,
) (to emu.ScreenLine, ok bool) {
	if len(pattern.String()) == 0 {
		return
	}

	line, lineOk := m.Line(from.R)
	if !lineOk {
		return
	}

	if isForward {
		if (from.C < 0 && from.C != -1) || from.C >= len(line) {
			return
		}
	} else {
		if from.C < 0 || from.C > len(line) {
			return
		}
	}

	// Need to adjust the initial match based on the absolute position of
	// the cut line
	var origin int
	if isForward {
		origin = from.C + 1
		if origin >= len(line) {
			return
		}

		line = line[origin:]
	}

	row := from.R
	for {
		if isForward {
			match := re.FindLine(pattern, line)
			if match != nil {
				to.R = row
				to.C0 = match[0] + origin
				to.C1 = match[1] + origin
				to.Chars = line[match[0]:match[1]]
				ok = true
				return
			}
			row++
		} else {
			to, ok = getBackwardsMatch(
				from,
				line,
				row,
				re.FindAllLine(pattern, line),
			)
			if ok {
				return
			}
			row--
		}

		line, lineOk = m.Line(row)
		if !lineOk {
			return
		}
		origin = 0
	}
}
