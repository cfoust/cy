package replay

import (
	"strings"
	"testing"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"

	"github.com/stretchr/testify/require"
)

func TestTimeModeProgressBarFullAtEnd(t *testing.T) {
	r, run := createTest(createTestSession())

	size := geom.Size{R: 10, C: 80}
	run(size, ActionEnd)

	state := tty.New(size)
	r.View(state)

	row := state.Image[state.Image.Size().R-1].String()
	start := strings.LastIndex(row, "[")
	end := strings.LastIndex(row, "]")
	require.Greater(t, start, -1)
	require.Greater(t, end, start)

	bar := row[start+1 : end]
	require.Contains(t, bar, "▒")
	require.NotContains(t, bar, "-")
}
