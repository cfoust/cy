package re

import (
	"io"
	"regexp"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/sessions/search"
)

type imageReader struct {
	i    image.Image
	from geom.Vec2
	to   geom.Vec2
	next geom.Vec2
}

var _ reader = (*imageReader)(nil)

func (l *imageReader) Next() (glyph emu.Glyph, loc geom.Vec2, done bool) {
	var (
		next = l.next
		i    = l.i
	)
	if next.R >= len(i) || next.R >= l.to.R {
		done = true
		return
	}

	line := i[next.R]
	if next.C >= len(line) {
		done = true
		return
	}

	glyph = line[next.C]
	loc = next
	l.next.C += glyph.Width()

	if l.next.C < l.to.C {
		return
	}

	l.next.C = l.from.C
	l.next.R++
	return
}

func (l *imageReader) Reset() {
	l.next = l.from
}

func getMatchIndex(pattern *regexp.Regexp) int {
	for i, subName := range pattern.SubexpNames() {
		if subName == "match" {
			return i
		}
	}

	return -1
}

func findAllSubmatch(pattern *regexp.Regexp) func(r io.RuneReader) [][]int {
	var (
		matchIndex  = getMatchIndex(pattern)
		hasNamed    = matchIndex != -1
		startOffset = 0
	)

	if hasNamed {
		startOffset = matchIndex * 2
	}

	return func(r io.RuneReader) [][]int {
		var (
			loc = pattern.FindReaderSubmatchIndex(r)
		)

		if startOffset >= len(loc) {
			return nil
		}

		matchSlice := loc[startOffset : startOffset+2]
		if len(loc) == 2 || hasNamed {
			return [][]int{matchSlice}
		}

		// Just take all submatches rather than whole pattern if that's
		// what's returned
		results := make([][]int, 0, (len(loc)-2)/2)
		for i := 2; i+1 < len(loc); i += 2 {
			results = append(
				results,
				loc[i:i+2],
			)
		}

		return results
	}
}

// FindAllImage finds all matches of `pattern` in `i` in the region specified by
// `from` and `to`. `to` is exclusive.
func FindAllImage(
	pattern *regexp.Regexp,
	i image.Image,
	from, to geom.Vec2,
) (loc []search.Selection) {
	r := &imageReader{
		i:    i,
		from: from,
		next: from,
		to:   to,
	}
	g := &glyphReader{reader: r}

	matches := findAll(findAllSubmatch(pattern), g, -1)
	if len(matches) == 0 {
		return nil
	}

	return translateMatches(r, matches)
}
