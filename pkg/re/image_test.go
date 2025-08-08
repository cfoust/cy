package re

import (
	"regexp"
	"testing"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/sessions/search"

	"github.com/stretchr/testify/require"
)

func TestFindImage(t *testing.T) {
	term := emu.New(emu.WithSize(geom.Size{
		R: 3,
		C: 10,
	}))
	for range 2 {
		term.Write([]byte("foobarbazf"))
	}
	term.Write([]byte("你还好吗"))

	i := image.Capture(term)

	for _, tc := range []struct {
		name     string
		from     geom.Vec2
		to       geom.Vec2
		pattern  *regexp.Regexp
		expected []search.Selection
	}{
		{
			name:    "two at beginning",
			from:    geom.Vec2{},
			to:      geom.Vec2{R: 1, C: 4},
			pattern: regexp.MustCompile("foo"),
			expected: []search.Selection{
				{
					From: geom.Vec2{R: 0, C: 0},
					To:   geom.Vec2{R: 0, C: 3},
				},
				{
					From: geom.Vec2{R: 1, C: 0},
					To:   geom.Vec2{R: 1, C: 3},
				},
			},
		},
		{
			name:    "bar on entire window",
			from:    geom.Vec2{},
			to:      geom.Vec2{R: 1, C: 10},
			pattern: regexp.MustCompile("bar"),
			expected: []search.Selection{
				{
					From: geom.Vec2{R: 0, C: 3},
					To:   geom.Vec2{R: 0, C: 6},
				},
				{
					From: geom.Vec2{R: 1, C: 3},
					To:   geom.Vec2{R: 1, C: 6},
				},
			},
		},
		{
			name:    "just in second row",
			from:    geom.Vec2{R: 1, C: 0},
			to:      geom.Vec2{R: 1, C: 10},
			pattern: regexp.MustCompile("bar"),
			expected: []search.Selection{
				{
					From: geom.Vec2{R: 1, C: 3},
					To:   geom.Vec2{R: 1, C: 6},
				},
			},
		},
		{
			name:    "cjk",
			from:    geom.Vec2{R: 0, C: 0},
			to:      geom.Vec2{R: 3, C: 10},
			pattern: regexp.MustCompile("还"),
			expected: []search.Selection{
				{
					From: geom.Vec2{R: 2, C: 2},
					To:   geom.Vec2{R: 2, C: 4},
				},
			},
		},
		{
			name:    "across rows",
			from:    geom.Vec2{R: 0, C: 0},
			to:      geom.Vec2{R: 1, C: 10},
			pattern: regexp.MustCompile("ffoo"),
			expected: []search.Selection{
				{
					From: geom.Vec2{R: 0, C: 9},
					To:   geom.Vec2{R: 1, C: 3},
				},
			},
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			require.Equal(
				t,
				tc.expected,
				FindAllImage(tc.pattern, i, tc.from, tc.to),
			)
		})
	}
}
