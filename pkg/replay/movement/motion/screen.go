package motion

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
)

// GotoColumn goes to the screen column in the current line.
func GotoColumn(m Movable, count int) {
	// TODO(cfoust): 06/12/24
}

func getCurrentLine(m Movable) (line emu.ScreenLine, ok bool) {
	screen, cursor := m.Viewport()
	if cursor.R < 0 || cursor.R >= len(screen) {
		return
	}
	return screen[cursor.R], true
}

// StartOfScreenLine corresponds to vim's `g0`.
func StartOfScreenLine(m Movable) {
	line, ok := getCurrentLine(m)
	if !ok {
		return
	}
	m.Goto(geom.Vec2{
		R: line.R,
		C: line.C0,
	})
}
