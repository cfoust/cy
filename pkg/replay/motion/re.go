package motion

import (
	"io"
	"regexp"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
)

func makePattern(pattern string) *regexp.Regexp {
	return regexp.MustCompile(pattern)
}

// An io.RuneReader that provides a sequence of runes corresponding to the
// cells in an emu.Line.
type LineReader struct {
	line emu.Line
	next int
}

var _ io.RuneReader = (*LineReader)(nil)

func (s *LineReader) ReadRune() (r rune, size int, err error) {
	next := s.next
	if next >= len(s.line) {
		return 0, 0, io.EOF
	}

	r = s.line[next].Char
	s.next += s.line[next].Width()
	size = len([]byte(string(r)))
	return
}

// translateMatch turns the result from FindReaderIndex into one that matches
// the indices of the emu.Line, since FindReaderIndex returns byte indices.
func translateMatch(src []int, line emu.Line) {
	var index, j int
	for i := 0; i < len(line) && j < 2; i++ {
		if index == src[j] {
			src[j] = i
			j++
		}

		index += len([]byte(string(line[i].Char)))
		i += line[i].Width() - 1
	}

	// This handles the case where the match was at the end of the line
	if j == 1 && index == src[j] {
		src[j] = len(line)
	}
}

// findLine looks for a single match of `re` in `line`.
func findLine(re *regexp.Regexp, line emu.Line) (loc []int) {
	l := &LineReader{line: line}
	loc = re.FindReaderIndex(l)
	if loc == nil {
		return
	}

	translateMatch(loc, line)
	return
}

// findAllLine returns all matches of `re` in `line`.
func findAllLine(re *regexp.Regexp, line emu.Line) (loc [][]int) {
	var i int
	for i < len(line) {
		match := findLine(re, line[i:])
		if match == nil {
			return
		}

		// Make match relative to full line
		for j := 0; j < len(match); j++ {
			match[j] += i
		}

		loc = append(loc, match)
		i = match[1]
	}
	return
}

// getBackwardsMatch finds the closest match on `line` behind `from`.
func getBackwardsMatch(
	from geom.Vec2,
	line emu.Line,
	row int,
	matches [][]int,
) (to emu.ScreenLine, ok bool) {
	if matches == nil || len(matches) == 0 {
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
	re *regexp.Regexp,
	from geom.Vec2,
	isForward bool,
) (to emu.ScreenLine, ok bool) {
	if len(re.String()) == 0 {
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
			match := findLine(re, line)
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
				findAllLine(re, line),
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
