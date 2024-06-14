package motion

import (
	"io"
	"regexp"

	"github.com/cfoust/cy/pkg/emu"
)

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
