package style

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/janet"

	"github.com/charmbracelet/lipgloss"
	"github.com/muesli/termenv"
)

type Color struct {
	lipgloss.Color
}

var _ janet.Unmarshalable = (*Color)(nil)

func (c *Color) UnmarshalJanet(value *janet.Value) (err error) {
	var str string
	err = value.Unmarshal(&str)
	if err != nil {
		return err
	}

	// TODO(cfoust): 08/06/24 validate colors?
	c.Color = lipgloss.Color(str)
	return nil
}

var _ janet.Marshalable = (*Color)(nil)

func (c *Color) MarshalJanet() interface{} {
	if c == nil {
		return nil
	}

	return string(c.Color)
}

func lipglossToEmu(l lipgloss.Color) emu.Color {
	switch c := renderer.ColorProfile().Color(string(l)).(type) {
	case termenv.ANSIColor:
		return emu.ANSIColor(int(c))
	case termenv.ANSI256Color:
		return emu.XTermColor(int(c))
	case termenv.RGBColor:
		r, g, b, _ := termenv.ConvertToRGB(c).RGBA()
		return emu.RGBColor(int(r), int(g), int(b))
	}

	return emu.DefaultFG
}

func (c *Color) Emu() emu.Color {
	return lipglossToEmu(c.Color)
}

func NewColor(c string) *Color {
	return &Color{Color: lipgloss.Color(c)}
}
