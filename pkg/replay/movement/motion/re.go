package motion

import (
	"io"
	"regexp"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
)

func makePattern(pattern string) *regexp.Regexp {
	compiled, err := regexp.Compile(pattern)
	if err != nil {
		panic(err)
	}
	return compiled
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

// FindNext gets the next match of a regex pattern in the given direction.
func FindNext(
	m Movable,
	re *regexp.Regexp,
	from geom.Vec2,
	isForward bool,
) (to geom.Vec2, ok bool) {
	line, ok := m.Line(from.R)
	if !ok {
		return
	}

	if from.C < 0 || from.C >= len(line) {
		return
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
	} else {
		// TODO(cfoust): 06/14/24 it's ok for match to start before origin but continue past
		line = line[:from.C]
	}

	var lineOk bool
	row := from.R
	for {
		if isForward {
			match := findLine(re, line)
			if match != nil {
				to.R = row
				to.C = match[0] + origin
				ok = true
				return
			}
			row++
		} else {
			matches := findAllLine(re, line)
			if len(matches) != 0 {
				to.R = row
				to.C = matches[len(matches)-1][0]
				ok = true
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
