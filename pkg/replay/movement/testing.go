package movement

import (
	"testing"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/params"
	"github.com/cfoust/cy/pkg/style"

	"github.com/stretchr/testify/require"
)

func TestHighlight(
	t *testing.T,
	m Movement,
	size geom.Size,
	highlights []Highlight,
	lines ...string,
) {
	bg := emu.ANSIColor(1)
	bgColor := style.Red
	testStyle := style.NewStyle(nil, bgColor)
	for i := range highlights {
		highlights[i].Style = testStyle
	}

	state := tty.New(size)
	m.View(
		params.New(),
		state,
		highlights,
	)
	image := state.Image

	for row := range lines {
		for col, char := range lines[row] {
			switch char {
			case '0':
				require.NotEqual(
					t,
					bg,
					image[row][col].BG,
					"cell [%d, %d] should not be highlighted",
					row,
					col,
				)
			case '1':
				require.Equal(
					t,
					bg,
					image[row][col].BG,
					"cell [%d, %d] should be highlighted",
					row,
					col,
				)
			default:
				t.Logf(
					"invalid char %+v, must be 1 or 0",
					char,
				)
				t.FailNow()
			}
		}
	}
}
