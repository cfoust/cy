package image

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
)

func (i *imageMovement) Cursor() geom.Vec2 {
	return i.viewportToTerm(i.cursor)
}

func (i *imageMovement) Goto(location geom.Vec2) {
	i.moveCursor(i.clampToTerminal(location))
}

func (i *imageMovement) Line(row int) (line emu.Line, ok bool) {
	screen := i.Screen()
	if row >= len(screen) || row < 0 {
		return
	}

	return screen[row], true
}
