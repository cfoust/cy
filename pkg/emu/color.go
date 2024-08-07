package emu

const (
	// We need to be able to distinguish colors that have never been
	// explicitly set.
	colorSet uint32 = 1 << 24
	// We also need to be able to distinguish between the background and
	// foreground colors--but _only_ when they have not yet been set.
	colorBG uint32 = 1 << 25
	// This flag determines whether the color should be interpreted as an
	// RGB color. The old method meant that all RGB colors within
	// 0x0000ff were represented incorrectly.
	colorRGB uint32 = 1 << 26

	colorAnsi  uint32 = 0xf
	colorXterm uint32 = 0xff
)

// Color maps to the ANSI colors [0, 16) and the xterm colors [16, 256).
type Color uint32

func ANSIColor(color int) Color {
	return Color(colorSet | (uint32(color) & colorAnsi))
}

func XTermColor(color int) Color {
	return Color(colorSet | (uint32(color) & colorXterm))
}

func RGBColor(r, g, b int) Color {
	return Color(colorSet | colorRGB | (uint32(r) << 16) | (uint32(g) << 8) | uint32(b))
}

// ANSI color values
const (
	Black Color = iota
	Red
	Green
	Yellow
	Blue
	Magenta
	Cyan
	LightGrey
	DarkGrey
	LightRed
	LightGreen
	LightYellow
	LightBlue
	LightMagenta
	LightCyan
	White
)

// Default colors are potentially distinct to allow for special behavior.
// For example, a transparent background. Otherwise, the simple case is to
// map default colors to another color.
const (
	DefaultFG Color = 0
	DefaultBG Color = Color(colorBG)
)

// Default reports whether this color has been set. If false, it represents
// the default foreground or background color.
func (c Color) Default() bool {
	return (uint32(c) & colorSet) == 0
}

// RGB reports whether Color is in the RGB color space.
func (c Color) RGB() (r, g, b int, ok bool) {
	if c.Default() {
		return
	}

	r = int((c >> 16) & 0xff)
	g = int((c >> 8) & 0xff)
	b = int(c & 0xff)
	ok = (uint32(c) & colorRGB) > 0
	return
}

// ANSI reports whether Color is within [0, 16) and returns that color if
// it is.
func (c Color) ANSI() (color int, ok bool) {
	if c.Default() || uint32(c)&colorXterm >= 16 {
		return
	}

	color = int(c & 0xf)
	ok = (uint32(c) & colorRGB) == 0
	return
}

// XTerm reports whether Color is within [0, 256) and returns that color if
// it is.
func (c Color) XTerm() (color int, ok bool) {
	if c.Default() {
		return
	}

	color = int(c & 0xff)
	ok = (uint32(c) & colorRGB) == 0
	return
}
