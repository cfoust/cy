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

func TestReleaseProgress(t *testing.T) {
	size := geom.Size{R: 10, C: 10}
	events := sessions.NewSimulator().
		Defaults().
		Add(
			"foo",
			size,
			"bar",
		).
		Events()

	p := New()
	p.Acquire()
	for _, event := range events {
		_ = p.Process(event)
	}

	type update struct {
		done  int
		total int
	}

	updates := make([]update, 0)
	p.ReleaseProgress(func(done, total int) {
		updates = append(updates, update{done: done, total: total})
	})

	require.False(t, p.getInUse())
	require.Len(t, p.Events(), len(events))

	require.GreaterOrEqual(t, len(updates), 2)
	require.Equal(t, update{done: 0, total: len(events)}, updates[0])
	require.Equal(t, update{done: len(events), total: len(events)}, updates[len(updates)-1])

	for i := 1; i < len(updates); i++ {
		require.GreaterOrEqual(t, updates[i].done, updates[i-1].done)
		require.GreaterOrEqual(t, updates[i].total, updates[i-1].total)
		require.LessOrEqual(t, updates[i].done, updates[i].total)
	}
}
