package motion

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
)

type ScreenLike interface {
	Cursor() geom.Vec2
	ScreenCursor() geom.Vec2
	Line(row int) (line emu.Line, ok bool)
	Screen(row int) (line emu.ScreenLine, ok bool)
	Goto(location geom.Vec2)
}
