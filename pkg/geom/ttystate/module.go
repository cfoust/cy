package ttystate

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom/image"
)

type TTYState struct {
	Image         image.Image
	Cursor        emu.Cursor
	CursorVisible bool
}

func New(columns, rows int) TTYState {
	return TTYState{
		Image:         image.New(columns, rows),
		CursorVisible: true,
	}
}

func Capture(view emu.View) TTYState {
	view.Lock()
	cursor := view.Cursor()
	cursorVisible := view.CursorVisible()
	view.Unlock()

	return TTYState{
		Image:         image.Capture(view),
		Cursor:        cursor,
		CursorVisible: cursorVisible,
	}
}
