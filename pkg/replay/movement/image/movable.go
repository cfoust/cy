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

func (i *imageMovement) Viewport() (lines []emu.ScreenLine, cursor geom.Vec2) {
	cursor = i.cursor

	var (
		screen   = i.Screen()
		offset   = i.offset
		viewport = i.viewport
	)

	lastRow := geom.Min(
		offset.R+viewport.R,
		len(screen),
	)
	for row := offset.R; row < lastRow; row++ {
		line := screen[row]
		lineEnd := geom.Min(len(line), offset.C+viewport.C)
		chars := line[offset.C:lineEnd]
		lines = append(
			lines,
			emu.ScreenLine{
				R:     row,
				C0:    offset.C,
				C1:    lineEnd,
				Chars: chars,
			},
		)
	}
	return
}
