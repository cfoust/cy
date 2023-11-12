package search

import (
	"io"
	"regexp"
	"testing"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/stretchr/testify/require"
)

func TestSequence(t *testing.T) {
	term := emu.New()
	term.Resize(4, 2)
	term.Write([]byte("foo"))

	reader := NewScreenReader(term, geom.Vec2{}, geom.Vec2{
		R: 2,
		C: 4,
	})

	sequence := []rune{
		// line 1
		'f',
		'o',
		'o',
		' ',

		// line 2
		' ',
		' ',
		' ',
		' ',
	}

	var r rune
	for _, s := range sequence {
		r, _, _ = reader.ReadRune()
		require.Equal(t, s, r)
	}

	_, _, err := reader.ReadRune()
	require.Error(t, err)
	require.Equal(t, io.EOF, err)
}

func TestSearcher(t *testing.T) {
	sim := sessions.NewSimulator().
		Add("foo").
		Add("f").
		Add(geom.DEFAULT_SIZE).
		Add("oo").
		Add("bar").
		Add("foo")

	pattern, _ := regexp.Compile("foo")
	s := NewSearcher()
	s.Parse(sim.Events())

	matches := s.Find(pattern)
	require.Equal(t, 3, len(matches))
	require.Equal(t, matches, []Match{
		{
			Begin: Address{
				Index:  0,
				Offset: 0,
			},
			End: Address{
				Index:  0,
				Offset: 3,
			},
			Continuous: true,
		},
		{
			Begin: Address{
				Index:  1,
				Offset: 0,
			},
			End: Address{
				Index:  3,
				Offset: 2,
			},
			Continuous: true,
		},
		{
			Begin: Address{
				Index:  5,
				Offset: 0,
			},
			End: Address{
				Index:  5,
				Offset: 3,
			},
			Continuous: true,
		},
	})
}
