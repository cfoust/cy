package thumbs

import (
	"regexp"
	"testing"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/sessions/search"

	"github.com/stretchr/testify/require"
)

func setWrite(
	i image.Image,
	write int,
	fromRow, fromCol, toRow, toCol int,
) {
	for col := fromCol; col < toCol; col++ {
		for row := fromRow; row < toRow; row++ {
			i[row][col].Write = emu.WriteID(write)
		}
	}
}

func TestRegions(t *testing.T) {
	size := geom.Vec2{
		R: 8,
		C: 8,
	}
	i := image.New(size)

	for count := range 2 {
		col := count * 4
		setWrite(i, count+1, 0, col, 8, col+4)
	}

	require.Equal(t,
		[]search.Selection{
			{
				To: size,
			},
			{
				From: geom.Vec2{R: 0, C: 0},
				To:   geom.Vec2{R: 8, C: 4},
			},
			{
				From: geom.Vec2{R: 0, C: 4},
				To:   geom.Vec2{R: 8, C: 8},
			},
		},
		getRegions(i),
	)
}

func TestConvert(t *testing.T) {
	region := search.Selection{
		From: geom.Vec2{},
		To:   geom.Vec2{R: 3, C: 3},
	}

	for _, tc := range []struct {
		name     string
		match    search.Selection
		expected []geom.Vec2
	}{
		{
			name: "basic",
			match: search.Selection{
				From: geom.Vec2{},
				To:   geom.Vec2{R: 0, C: 3},
			},
			expected: []geom.Vec2{
				{R: 0, C: 0},
				{R: 0, C: 1},
				{R: 0, C: 2},
			},
		},
		{
			name: "wrap",
			match: search.Selection{
				From: geom.Vec2{R: 0, C: 2},
				To:   geom.Vec2{R: 1, C: 1},
			},
			expected: []geom.Vec2{
				{R: 0, C: 2},
				{R: 1, C: 0},
			},
		},
		{
			name: "first line",
			match: search.Selection{
				From: geom.Vec2{R: 0, C: 0},
				To:   geom.Vec2{R: 1, C: 0},
			},
			expected: []geom.Vec2{
				{R: 0, C: 0},
				{R: 0, C: 1},
				{R: 0, C: 2},
			},
		},
		{
			name: "wrap_twice",
			match: search.Selection{
				// 0 0 1
				// 1 1 1
				// 1 0 0
				From: geom.Vec2{R: 0, C: 2},
				To:   geom.Vec2{R: 2, C: 1},
			},
			expected: []geom.Vec2{
				{R: 0, C: 2},
				{R: 1, C: 0},
				{R: 1, C: 1},
				{R: 1, C: 2},
				{R: 2, C: 0},
			},
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			require.Equal(
				t,
				thumbMatch(tc.expected),
				convertMatch(region, tc.match),
			)
		})
	}
}

func pattern(s string) *regexp.Regexp {
	return regexp.MustCompile(s)
}

func TestFind(t *testing.T) {
	term := emu.New(emu.WithSize(geom.Size{
		R: 3,
		C: 10,
	}))
	for range 2 {
		term.Write([]byte("foobarbazf"))
	}
	term.Write([]byte("你还好吗"))

	i := image.Capture(term)

	// foo is one pane
	setWrite(i, 1,
		0, 0,
		3, 3,
	)
	// baz is another
	setWrite(i, 2,
		0, 6,
		3, 9,
	)

	line := func(row, col int, text string) (result thumbMatch) {
		for index := range len(text) {
			result = append(result, geom.Vec2{
				R: row,
				C: col + index,
			})
		}
		return
	}

	merge := func(matches ...thumbMatch) (result thumbMatch) {
		for _, match := range matches {
			result = append(result, match...)
		}
		return
	}

	for _, tc := range []struct {
		name     string
		patterns []*regexp.Regexp
		expected []thumbMatch
	}{
		{
			name: "take longest",
			patterns: []*regexp.Regexp{
				pattern("foobar"),
				pattern("foo"),
			},
			expected: []thumbMatch{
				line(0, 0, "foobar"),
				line(1, 0, "foobar"),
			},
		},
		{
			name: "take longest with submatch",
			patterns: []*regexp.Regexp{
				pattern("foobar"),
				pattern("bar"),
			},
			expected: []thumbMatch{
				line(0, 0, "foobar"),
				line(1, 0, "foobar"),
			},
		},
		{
			name: "wrapping in two regions",
			patterns: []*regexp.Regexp{
				pattern("foofoo"),
				pattern("bazbaz"),
			},
			expected: []thumbMatch{
				merge(
					line(0, 0, "foo"),
					line(1, 0, "foo"),
				),
				merge(
					line(0, 6, "baz"),
					line(1, 6, "baz"),
				),
			},
		},
		{
			name: "takes longest across two regions",
			patterns: []*regexp.Regexp{
				pattern("foofoo"),
				pattern("bazbaz"),
				pattern("foobarbaz"),
			},
			expected: []thumbMatch{
				line(0, 0, "foobarbaz"),
				line(1, 0, "foobarbaz"),
			},
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			require.Equal(
				t,
				tc.expected,
				findPatterns(tc.patterns, i),
			)
		})
	}
}
