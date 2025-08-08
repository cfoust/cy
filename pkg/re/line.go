package re

import (
	"io"
	"regexp"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
)

type lineReader struct {
	lines []emu.Line
	next  geom.Vec2
}

var _ reader = (*lineReader)(nil)

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

func findAllLine(
	pattern *regexp.Regexp,
	line emu.Line,
	count int,
) (loc [][]int) {
	r := &lineReader{lines: []emu.Line{line}}
	g := &glyphReader{reader: r}

	matches := findAll(func(r io.RuneReader) [][]int {
		loc := pattern.FindReaderIndex(r)
		if len(loc) == 0 {
			return nil
		}

		return [][]int{loc}
	}, g, count)
	if len(matches) == 0 {
		return nil
	}

	for _, match := range translateMatches(r, matches) {
		loc = append(loc, []int{match.From.C, match.To.C})
	}
	return
}

// FindLine looks for a single match of `re` in `line`.
func FindLine(pattern *regexp.Regexp, line emu.Line) (loc []int) {
	matches := findAllLine(pattern, line, 1)
	if len(matches) == 0 {
		return nil
	}

	return matches[0]
}

// FindAllLine returns all matches of `re` in `line`.
func FindAllLine(pattern *regexp.Regexp, line emu.Line) (loc [][]int) {
	return findAllLine(pattern, line, -1)
}
