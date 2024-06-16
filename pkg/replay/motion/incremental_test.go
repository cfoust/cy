package motion

import (
	"testing"

	"github.com/cfoust/cy/pkg/geom"

	"github.com/stretchr/testify/require"
)

func TestIncremental(t *testing.T) {
	m := fromLines(
		"foo bar baz",
		"foo 你好 baz",
		"foo bar baz",
	)
	m.Goto(geom.Vec2{})

	i := NewIncremental()
	i.Start(m, true)
	require.True(t, i.IsActive())
	require.Equal(t, geom.Vec2{}, i.origin)
}
