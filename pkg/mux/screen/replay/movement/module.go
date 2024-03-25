package movement

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
)

type Movement interface {
	HandleSeek()
	ScrollXDelta(delta int)
	ScrollYDelta(delta int)
	Resize(geom.Size)
	Cursor() geom.Vec2
	ReadString(start, end geom.Vec2) string
}

func getTerminalCursor(terminal emu.Terminal) geom.Vec2 {
	cursor := terminal.Cursor()
	return geom.Vec2{
		R: cursor.Y,
		C: cursor.X,
	}
}

func getTerminalSize(terminal emu.Terminal) geom.Vec2 {
	cols, rows := terminal.Size()
	return geom.Vec2{
		R: rows,
		C: cols,
	}
}
