package tty

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
)

type State struct {
	Image         image.Image
	Cursor        emu.Cursor
	CursorVisible bool
}

func New(size geom.Vec2) *State {
	return &State{
		Image:         image.New(size),
		CursorVisible: true,
	}
}

func Copy(pos geom.Vec2, dst, src *State) {
	image.Copy(pos, dst.Image, src.Image)

	dst.Cursor = src.Cursor
	dst.Cursor.X += pos.C
	dst.Cursor.Y += pos.R
}

func Capture(view emu.View) *State {
	view.Lock()
	cursor := view.Cursor()
	cursorVisible := view.CursorVisible()
	view.Unlock()

	return &State{
		Image:         image.Capture(view),
		Cursor:        cursor,
		CursorVisible: cursorVisible,
	}
}
