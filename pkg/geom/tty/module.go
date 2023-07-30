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

func (s *State) Clone() *State {
	cloned := New(s.Image.Size())
	cloned.Cursor = s.Cursor
	cloned.CursorVisible = s.CursorVisible
	return cloned
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
	cursor := view.Cursor()
	cursorVisible := view.CursorVisible()

	return &State{
		Image:         image.Capture(view),
		Cursor:        cursor,
		CursorVisible: cursorVisible,
	}
}
