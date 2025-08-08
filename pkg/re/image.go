package re

import (
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
	if next.R >= len(i) || next.R > l.to.R {
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

	matches := findAll(pattern, g, -1)
	if len(matches) == 0 {
		return nil
	}

	return translateMatches(r, matches)
}
