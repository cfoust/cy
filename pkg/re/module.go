package re

import (
	"io"
	"regexp"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/sessions/search"
)

func MakePattern(pattern string) *regexp.Regexp {
	return regexp.MustCompile(pattern)
}

type reader interface {
	Next() (glyph emu.Glyph, loc geom.Vec2, done bool)
	Reset()
}

type lineReader struct {
	lines []emu.Line
	next  geom.Vec2
}

func (l *lineReader) Next() (glyph emu.Glyph, loc geom.Vec2, done bool) {
	var (
		next  = l.next
		lines = l.lines
	)
	if next.R >= len(lines) {
		done = true
		return
	}

	line := lines[next.R]
	if next.C >= len(line) {
		done = true
		return
	}

	glyph = line[next.C]
	loc = l.next
	l.next.C += glyph.Width()
	if l.next.C < len(line) {
		return
	}

	l.next.C = 0
	l.next.R++
	return
}

func (l *lineReader) Reset() {
	l.next = geom.Vec2{}
}

// glyphReader is an io.RuneReader that provides a sequence of runes
// corresponding to the cells in an array of emu.Line.
type glyphReader struct {
	r reader
}

var _ io.RuneReader = (*glyphReader)(nil)

func (s *glyphReader) ReadRune() (r rune, size int, err error) {
	glyph, _, done := s.r.Next()
	if done {
		return 0, 0, io.EOF
	}
	r = glyph.Char
	size = len([]byte(string(r)))
	return
}

func translateMatches(r reader, matches [][]int) (results []search.Selection) {
	results = make([]search.Selection, len(matches))

	var (
		starts = make(map[int][]int)
		ends   = make(map[int][]int)
	)

	for i, match := range matches {
		start := match[0]
		end := match[1]
		starts[start] = append(starts[start], i)
		ends[end] = append(ends[end], i)
	}

	var index int
	var last geom.Vec2
	for {
		glyph, loc, done := r.Next()
		if done {
			break
		}

		if indices, ok := starts[index]; ok {
			for match := range indices {
				results[match].From = loc
			}
		}

		if indices, ok := ends[index]; ok {
			for match := range indices {
				results[match].To = loc
			}
		}
		index += len([]byte(string(glyph.Char)))
		last = loc
	}

	// Handles the case where the match was at the end of the line
	last.C++
	if indices, ok := ends[index]; ok {
		for match := range indices {
			results[match].To = last
		}
	}

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

// FindLine looks for a single match of `re` in `line`.
func FindLine(re *regexp.Regexp, line emu.Line) (loc []int) {
	r := &lineReader{lines: []emu.Line{line}}
	l := &glyphReader{r: r}
	loc = re.FindReaderIndex(l)
	if loc == nil {
		return
	}

	r.Reset()
	match := translateMatches(r, [][]int{loc})[0]
	return []int{match.From.C, match.To.C}
}

// FindAllLine returns all matches of `re` in `line`.
func FindAllLine(re *regexp.Regexp, line emu.Line) (loc [][]int) {
	var i int
	for i < len(line) {
		match := FindLine(re, line[i:])
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
