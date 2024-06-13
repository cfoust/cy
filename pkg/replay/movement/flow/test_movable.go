package flow

import (
	"testing"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/sessions"

	"github.com/stretchr/testify/require"
)

func TestMovable(t *testing.T) {
	s := sessions.
		NewSimulator().
		Add(
			geom.Size{R: 4, C: 4},
			emu.LineFeedMode,
			"foo bar\nbaz",
		).
		Terminal()

	r := createFlowTest(s, geom.Size{R: 3, C: 4})

	// Viewport() I
	{
		r.scrollToLine(geom.Size{R: 1, C: 0}, ScrollPositionTop)
		_, size, cursor := r.Viewport()
		require.Equal(t, geom.Size{R: 0, C: 2}, cursor)
		require.Equal(t, geom.Size{R: 1, C: 2}, r.Cursor())
		require.Equal(t, geom.Size{R: 3, C: 4}, size)
		r.scrollToLine(geom.Size{R: 1, C: 0}, ScrollPositionCenter)
	}

	// Viewport() II
	{
		lines, size, cursor := r.Viewport()
		require.Equal(t, geom.Size{R: 0, C: 2}, cursor)
		require.Equal(t, geom.Size{R: 3, C: 4}, size)

		for _, line := range lines {
			line.Chars = nil
		}

		require.Equal(t, []emu.ScreenLine{
			{
				R:  0,
				C0: 0,
				C1: 4,
			},
			{
				R:  0,
				C0: 4,
				C1: 7,
			},
			{
				R:  1,
				C0: 0,
				C1: 3,
			},
		}, lines)
	}

	// Cursor()
	{
		cursor := r.Cursor()
		// this is not the same thing as the cursor returned by Viewport(), but it
		// coincidentally looks like it is
		require.Equal(t, geom.Size{R: 1, C: 2}, cursor)
	}

	// Line()
	{
		line, ok := r.Line(1)
		require.True(t, ok)
		require.Equal(t, emu.LineFromString("bar"), line)
	}

	// Goto()
	{
		r.Goto(geom.Size{R: 0, C: 2})
		require.Equal(t, geom.Size{R: 0, C: 2}, r.Cursor())
	}
}
