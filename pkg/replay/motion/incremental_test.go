package motion

import (
	"testing"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"

	"github.com/stretchr/testify/require"
)

func TestIncremental(t *testing.T) {
	m := fromLines(
		"foo bar baz one[",
	)
	m.Goto(geom.Vec2{})

	i := NewIncremental()

	i.Start(m, true)
	require.True(t, i.IsActive())
	require.True(t, i.isForward)
	require.Equal(t, geom.Vec2{}, i.origin)

	// Try typing a simple pattern forwards
	{
		i.Pattern(m, "bar")
		line, ok := i.Highlight()
		require.Equal(t, geom.Vec2{R: 0, C: 4}, m.Cursor())
		require.True(t, ok)
		require.Equal(t,
			emu.ScreenLine{
				R:     0,
				C0:    4,
				C1:    7,
				Chars: emu.LineFromString("bar"),
			},
			line,
		)
	}

	// Then cancel it
	{
		i.Cancel(m)
		require.False(t, i.IsActive())
		require.Equal(t, geom.Vec2{R: 0, C: 0}, m.Cursor())
	}

	// Try typing a simple pattern backwards
	{
		m.Goto(geom.Vec2{R: 0, C: 4})
		i.Start(m, false)
		i.Pattern(m, "foo")
		line, ok := i.Highlight()
		require.Equal(t, geom.Vec2{R: 0, C: 0}, m.Cursor())
		require.True(t, ok)
		require.Equal(t,
			emu.ScreenLine{
				R:     0,
				C0:    0,
				C1:    3,
				Chars: emu.LineFromString("foo"),
			},
			line,
		)
	}

	// Then cancel it
	{
		i.Cancel(m)
		require.False(t, i.IsActive())
		require.Equal(t, geom.Vec2{R: 0, C: 4}, m.Cursor())
	}

	// Invalid regex pattern
	{
		m.Goto(geom.Vec2{R: 0, C: 0})
		i.Start(m, true)
		i.Pattern(m, "ne[")
		line, ok := i.Highlight()
		require.Equal(t, geom.Vec2{R: 0, C: 13}, m.Cursor())
		require.True(t, ok)
		require.Equal(t,
			emu.ScreenLine{
				R:     0,
				C0:    13,
				C1:    16,
				Chars: emu.LineFromString("ne["),
			},
			line,
		)
		i.Cancel(m)
	}

	// Valid regex pattern
	{
		m.Goto(geom.Vec2{R: 0, C: 0})
		i.Start(m, true)
		i.Pattern(m, "o[a-z]e")
		line, ok := i.Highlight()
		require.Equal(t, geom.Vec2{R: 0, C: 12}, m.Cursor())
		require.True(t, ok)
		require.Equal(t,
			emu.ScreenLine{
				R:     0,
				C0:    12,
				C1:    15,
				Chars: emu.LineFromString("one"),
			},
			line,
		)
		i.Cancel(m)
	}

	// Next forwards
	{
		m.Goto(geom.Vec2{R: 0, C: 0})
		i.Start(m, true)
		i.Pattern(m, "ba")
		i.Accept()
		require.Equal(t, geom.Vec2{R: 0, C: 4}, m.Cursor())
		i.Next(m, true)
		require.Equal(t, geom.Vec2{R: 0, C: 8}, m.Cursor())
		// wrap around
		i.Next(m, true)
		require.Equal(t, geom.Vec2{R: 0, C: 4}, m.Cursor())
		// wrap around backwards
		i.Next(m, false)
		require.Equal(t, geom.Vec2{R: 0, C: 8}, m.Cursor())
	}

	// Next backwards
	{
		m.Goto(geom.Vec2{R: 0, C: 0})
		i.Start(m, false)
		i.Pattern(m, "ba")
		i.Accept()
		require.Equal(t, geom.Vec2{R: 0, C: 8}, m.Cursor())
		i.Next(m, true)
		require.Equal(t, geom.Vec2{R: 0, C: 4}, m.Cursor())
		// wrap around
		i.Next(m, true)
		require.Equal(t, geom.Vec2{R: 0, C: 8}, m.Cursor())
		// wrap around backwards
		i.Next(m, false)
		require.Equal(t, geom.Vec2{R: 0, C: 4}, m.Cursor())
	}
}
