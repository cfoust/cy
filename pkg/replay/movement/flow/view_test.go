package flow

import (
	"fmt"
	"strings"
	"testing"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/params"

	"github.com/stretchr/testify/require"
)

func TestFlowLineIndicatorShowsQuestionMarkWhenHistoryPruned(t *testing.T) {
	termSize := geom.Size{R: 3, C: 40}

	term := emu.New(emu.WithHistoryLimit(5))
	term.Resize(termSize)
	_, _ = term.Write([]byte(emu.LineFeedMode))

	for i := 0; i < 20; i++ {
		_, _ = fmt.Fprintf(term, "line %02d\n", i)
	}

	f := createFlowTest(term, termSize)

	screenRoot := f.Root()
	history := f.History()
	firstLine := screenRoot.R - len(history)
	if len(history) > 0 && history[len(history)-1].IsWrapped() {
		firstLine++
	}
	require.Greater(t, firstLine, 0)

	f.ScrollYDelta(-1)
	require.Less(t, f.root.R, f.Root().R)

	state := tty.New(termSize)
	f.View(params.New(), state, nil)

	row := state.Image[0].String()
	require.Contains(t, row, "[")
	require.True(t, strings.Contains(row, "?]"), row)
}

func TestFlowLineIndicatorDoesNotShowQuestionMarkWhenHistoryNotPruned(
	t *testing.T,
) {
	termSize := geom.Size{R: 3, C: 40}

	term := emu.New(emu.WithHistoryLimit(1000))
	term.Resize(termSize)
	_, _ = term.Write([]byte(emu.LineFeedMode))

	for i := 0; i < 20; i++ {
		_, _ = fmt.Fprintf(term, "line %02d\n", i)
	}

	f := createFlowTest(term, termSize)

	screenRoot := f.Root()
	history := f.History()
	firstLine := screenRoot.R - len(history)
	if len(history) > 0 && history[len(history)-1].IsWrapped() {
		firstLine++
	}
	require.Equal(t, 0, firstLine)

	f.ScrollYDelta(-1)
	require.Less(t, f.root.R, f.Root().R)

	state := tty.New(termSize)
	f.View(params.New(), state, nil)

	row := state.Image[0].String()
	require.Contains(t, row, "[")
	require.NotContains(t, row, "?]")
}
