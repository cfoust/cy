package motion

import (
	"testing"

	"github.com/cfoust/cy/pkg/geom"

	"github.com/stretchr/testify/require"
)

func TestWord(t *testing.T) {
	m := fromLines(
		"foo bar baz",
		"",
		"  ",
		"foo",
	)

	{
		Word(m, true, false)
		require.Equal(t, geom.Vec2{R: 0, C: 4}, m.Cursor())

		Word(m, false, false)
		require.Equal(t, geom.Vec2{R: 0, C: 0}, m.Cursor())

		Word(m, true, false)
		Word(m, true, false)
		require.Equal(t, geom.Vec2{R: 0, C: 8}, m.Cursor())

		// next line
		Word(m, true, false)
		require.Equal(t, geom.Vec2{R: 1, C: 0}, m.Cursor())

		// Should skip row 2
		Word(m, true, false)
		require.Equal(t, geom.Vec2{R: 3, C: 0}, m.Cursor())

		// Back
		Word(m, false, false)
		require.Equal(t, geom.Vec2{R: 1, C: 0}, m.Cursor())
	}

	{
		m.Goto(geom.Vec2{})

		Word(m, true, true)
		require.Equal(t, geom.Vec2{R: 0, C: 2}, m.Cursor())

		Word(m, true, true)
		require.Equal(t, geom.Vec2{R: 0, C: 6}, m.Cursor())

		Word(m, false, true)
		require.Equal(t, geom.Vec2{R: 0, C: 2}, m.Cursor())
	}
}
