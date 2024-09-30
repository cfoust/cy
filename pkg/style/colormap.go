package style

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/janet"

	"github.com/charmbracelet/lipgloss"
)

type ColorMap struct {
	mapping map[lipgloss.Color]lipgloss.Color
}

var _ janet.Unmarshalable = (*ColorMap)(nil)

func (c *ColorMap) UnmarshalJanet(value *janet.Value) (err error) {
	var newMapping map[string]string
	err = value.Unmarshal(&newMapping)
	if err != nil {
		return err
	}

	c.mapping = make(
		map[lipgloss.Color]lipgloss.Color,
		len(newMapping),
	)

	for k, v := range newMapping {
		c.mapping[lipgloss.Color(k)] = lipgloss.Color(v)
	}

	return nil
}

var _ janet.Marshalable = (*Color)(nil)

func (c *ColorMap) MarshalJanet() interface{} {
	if c == nil {
		return nil
	}

	mapping := make(map[string]string, len(c.mapping))
	for k, v := range c.mapping {
		mapping[string(k)] = string(v)
	}

	return mapping
}

// Apply applies the color mapping to the Image.
func (c *ColorMap) Apply(i image.Image) {
	if len(c.mapping) == 0 {
		return
	}

	size := i.Size()

	emuMapping := make(map[emu.Color]emu.Color, len(c.mapping))
	for k, v := range c.mapping {
		emuMapping[lipglossToEmu(k)] = lipglossToEmu(v)
	}

	fg, haveFg := emuMapping[emu.ANSIColor(7)]
	bg, haveBg := emuMapping[emu.ANSIColor(0)]

	for row := 0; row < size.R; row++ {
		for col := 0; col < size.C; col++ {
			if i[row][col].FG.Default() && haveFg {
				i[row][col].FG = fg
			} else if newFg, ok := emuMapping[i[row][col].FG]; ok {
				i[row][col].FG = newFg
			}

			if i[row][col].BG.Default() && haveBg {
				i[row][col].BG = bg
			} else if newBg, ok := emuMapping[i[row][col].BG]; ok {
				i[row][col].BG = newBg
			}
		}
	}
}
