package search

import (
	"io"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"

	"github.com/mattn/go-runewidth"
)

// An io.RuneReader that provides a sequence of runes corresponding to the
// cells on the screen of an emu.Terminal. Runes are read from top to bottom,
// left to right, starting at the ScreenReader's initial cell.
type ScreenReader struct {
	term    emu.Terminal
	start   geom.Vec2
	next    geom.Vec2
	numRead int
}

var _ io.RuneReader = (*ScreenReader)(nil)

// Reset the position of the ScreenReader.
func (s *ScreenReader) Reset(next geom.Vec2) {
	s.start = next
	s.next = next
	s.numRead = 0
}

func (s *ScreenReader) NumRead() int {
	return s.numRead
}

func (s *ScreenReader) ReadRune() (r rune, size int, err error) {
	cols, rows := s.term.Size()
	next := s.next
	if next.R >= rows || next.R < 0 || next.C < 0 || next.C > cols {
		return 0, 0, io.EOF
	}

	s.numRead++
	cell := s.term.Cell(next.C, next.R)
	r = cell.Char
	size = len([]byte(string(r)))

	w := runewidth.RuneWidth(r)
	s.next.C += w

	// still on same line
	if s.next.C < cols {
		return
	}

	// move to next line otherwise
	s.next.R++
	s.next.C = 0

	return
}

func NewScreenReader(term emu.Terminal, start geom.Vec2) *ScreenReader {
	return &ScreenReader{
		term:  term,
		start: start,
		next:  start,
	}
}
