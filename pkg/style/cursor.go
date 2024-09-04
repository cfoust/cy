package style

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom/image"
)

// GhostCursor adds a (standardized) "ghost" cursor to an image. This shows the
// user where the cursor actually is.
func GhostCursor(image image.Image, row, col int) {
	image[row][col].BG = emu.ANSIColor(8)
}
