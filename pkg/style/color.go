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
		r, g, b := termenv.ConvertToRGB(c).RGB255()
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

var (
	Black        = NewColor("0")
	Red          = NewColor("1")
	Green        = NewColor("2")
	Yellow       = NewColor("3")
	Blue         = NewColor("4")
	Magenta      = NewColor("5")
	Cyan         = NewColor("6")
	LightGrey    = NewColor("7")
	DarkGrey     = NewColor("8")
	LightRed     = NewColor("9")
	LightGreen   = NewColor("10")
	LightYellow  = NewColor("11")
	LightBlue    = NewColor("12")
	LightMagenta = NewColor("13")
	LightCyan    = NewColor("14")
	White        = NewColor("15")
)
