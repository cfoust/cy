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

func findLine(re *regexp.Regexp, line emu.Line) (loc []int) {
	l := &LineReader{line: line}
	loc = re.FindReaderIndex(l)
	if loc == nil {
		return
	}

	translateMatch(loc, line)
	return
}
