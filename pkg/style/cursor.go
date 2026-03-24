package style

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom/image"

	"github.com/lucasb-eyer/go-colorful"
)

// dimColor returns a dimmed variant of the given color. For ANSI
// colors 0-7, returns them as-is. For bright ANSI colors 8-15,
// returns the standard counterpart (color - 8). For RGB colors,
// halves each channel. Returns ok=false for XTerm 16-255 palette
// colors which have no natural dim variant.
func dimColor(c emu.Color) (dimmed emu.Color, ok bool) {
	if ansi, isAnsi := c.ANSI(); isAnsi {
		if ansi < 8 {
			// 0-7 are already dim
			return c, true
		}
		// 8-15 bright -> 0-7 standard
		return emu.ANSIColor(ansi - 8), true
	}

	if r, g, b, isRGB := c.RGB(); isRGB {
		col := colorful.Color{
			R: float64(r) / 255,
			G: float64(g) / 255,
			B: float64(b) / 255,
		}
		h, s, l := col.Hsl()
		dim := colorful.Hsl(h, s, l*0.5)
		dr, dg, db := dim.Clamped().RGB255()
		return emu.RGBColor(
			int(dr), int(dg), int(db),
		), true
	}

	// XTerm 16-255 palette: no natural dim variant
	return c, false
}

// GhostCursor adds a (standardized) "ghost" cursor to an image.
// This shows the user where the cursor actually is. If cursorColor
// has been set via OSC 12 and has a dim variant, it is used;
// otherwise ANSI color 8 (dark gray) is the default.
func GhostCursor(
	image image.Image,
	row, col int,
	cursorColor emu.Color,
) {
	if cursorColor != emu.DefaultCursor {
		if dim, ok := dimColor(cursorColor); ok {
			image[row][col].BG = dim
			return
		}
	}
	image[row][col].BG = emu.ANSIColor(8)
}
