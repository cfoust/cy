package style

import (
	"fmt"
	"testing"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"

	"github.com/charmbracelet/lipgloss"
	"github.com/stretchr/testify/require"
)

func TestColorConvert(t *testing.T) {
	og := lipgloss.Color("#1d1f21")
	emuColor := renderer.LipglossToEmu(og)
	r, g, b, ok := emuColor.RGB()
	require.True(t, ok)
	require.Equal(t, 29, r)
	require.Equal(t, 31, g)
	require.Equal(t, 33, b)
}

func TestColormap(t *testing.T) {
	image := image.New(geom.DEFAULT_SIZE)

	for i := 0; i < 16; i++ {
		image[0][i].FG = emu.ANSIColor(i)
		image[0][i].BG = emu.ANSIColor(i)
	}

	colorMap := NewColorMap(map[lipgloss.Color]lipgloss.Color{
		// normal
		lipgloss.Color("0"): lipgloss.Color("#1f1f1f"),
		lipgloss.Color("1"): lipgloss.Color("#d6dbe5"),
		lipgloss.Color("2"): lipgloss.Color("#f3bd09"),
		lipgloss.Color("3"): lipgloss.Color("#1dd361"),
		lipgloss.Color("4"): lipgloss.Color("#5350b9"),
		lipgloss.Color("5"): lipgloss.Color("#0f7ddb"),
		lipgloss.Color("6"): lipgloss.Color("#1081d6"),
		lipgloss.Color("7"): lipgloss.Color("#4e5ab7"),

		// bright
		lipgloss.Color("8"):  lipgloss.Color("#ecba0f"),
		lipgloss.Color("9"):  lipgloss.Color("#de352e"),
		lipgloss.Color("10"): lipgloss.Color("#f81118"),
		lipgloss.Color("11"): lipgloss.Color("#2dc55e"),
		lipgloss.Color("12"): lipgloss.Color("#2a84d2"),
		lipgloss.Color("13"): lipgloss.Color("#1081d6"),
		lipgloss.Color("14"): lipgloss.Color("#ffffff"),
		lipgloss.Color("15"): lipgloss.Color("#d6dbe5"),
	})

	colorMap.Apply(image)

	for i := 0; i < 16; i++ {
		color := renderer.LipglossToEmu(colorMap.mapping[lipgloss.Color(
			fmt.Sprintf("%d", i),
		)])
		require.Equal(t, color, image[0][i].FG)
		require.Equal(t, color, image[0][i].BG)
	}
}
