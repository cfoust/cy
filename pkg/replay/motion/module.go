package motion

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
)

type Movable interface {
	Cursor() geom.Vec2
	Line(row int) (line emu.Line, ok bool)
	NumLines() int
	Goto(location geom.Vec2)
	Viewport() (
		screen []emu.ScreenLine,
		size geom.Vec2,
		cursor geom.Vec2,
	)
}

type Motion func(m Movable)
