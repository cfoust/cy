package player

import (
	"testing"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/sessions/search"

	"github.com/stretchr/testify/require"
)

func getLine(t emu.Terminal, index int) string {
	line := t.Screen()[index]
	return line[:line.Length()].String()
}

func TestTime(t *testing.T) {
	size := geom.Size{R: 10, C: 10}
	events := sessions.NewSimulator().
		Defaults().
		Add(
			"foo",         // 2
			size,          // 3
			"b", "a", "r", // 6
		).
		Events()

	p := FromEvents(events)

	// Partway through
	p.Goto(2, 1)
	require.Equal(t, "fo", getLine(p, 0))

	// Before the resize
	p.Goto(2, -1)
	require.Equal(t, geom.DEFAULT_SIZE, p.Size())
	require.Equal(t, "foo", getLine(p, 0))
	// After it
	p.Goto(3, -1)
	require.Equal(t, size, p.Size())

	// After everything
	p.Goto(6, -1)
	require.Equal(t, search.Address{Index: 6, Offset: 0}, p.location)
	require.Equal(t, p.nextDetect, 7)
	require.Equal(t, "foobar", getLine(p, 0))
}
