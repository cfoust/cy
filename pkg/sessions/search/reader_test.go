package search

import (
	"io"
	"testing"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
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
