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

type glyphReader struct {
	reader
	offset int
}

var _ io.RuneReader = (*glyphReader)(nil)

func (s *glyphReader) Reset() {
	s.offset = 0
	s.reader.Reset()
}

func (s *glyphReader) ReadRune() (r rune, size int, err error) {
	glyph, _, done := s.Next()
	if done {
		return 0, 0, io.EOF
	}
	r = glyph.Char
	size = len([]byte(string(r)))
	s.offset += size
	return
}

func translateMatches(
	r reader,
	matches [][]int,
) (results []search.Selection) {
	r.Reset()
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

	var offset int
	var last geom.Vec2
	for {
		glyph, loc, done := r.Next()
		if done {
			break
		}

		if indices, ok := starts[offset]; ok {
			for _, match := range indices {
				results[match].From = loc
			}
		}

		if indices, ok := ends[offset]; ok {
			for _, match := range indices {
				results[match].To = loc
			}
		}

		offset += len([]byte(string(glyph.Char)))
		last = loc
	}

	// Handles the case where the match was at the end of the line
	last.C++
	if indices, ok := ends[offset]; ok {
		for _, match := range indices {
			results[match].To = last
		}
	}

	return
}

func findAll(
	find func(io.RuneReader) [][]int,
	g *glyphReader,
	count int,
) (matches [][]int) {
	var last int
	for {
		// Reset the reader back to one cell past the beginning of the
		// last match
		if len(matches) > 0 {
			last = matches[len(matches)-1][0]

			g.Reset()
			for g.offset <= last {
				g.ReadRune()
			}
		}

		offset := g.offset
		loc := find(g)
		if len(loc) == 0 {
			return
		}

		for _, match := range loc {
			match[0] += offset
			match[1] += offset
			matches = append(matches, match)

			if count != -1 && len(matches) == count {
				return
			}
		}
	}
}
