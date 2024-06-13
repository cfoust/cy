package motion

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/rs/zerolog/log"
)

// GotoColumn goes to the screen column in the current line.
func GotoColumn(m Movable, count int) {
	// TODO(cfoust): 06/12/24
}

func getCurrentLine(m Movable) (line emu.ScreenLine, ok bool) {
	screen, _, cursor := m.Viewport()
	if cursor.R < 0 || cursor.R >= len(screen) {
		return
	}
	log.Info().Msgf("current %+v %+v", cursor, screen[cursor.R])
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

// MiddleOfScreenLine corresponds to vim's `gm`.
func MiddleOfScreenLine(m Movable) {
	_, size, _ := m.Viewport()
	line, ok := getCurrentLine(m)
	if !ok {
		return
	}

	log.Info().Msgf("moving to %+v", geom.Vec2{
		R: line.R,
		C: geom.Clamp(
			line.C0+size.C/2,
			line.C0,
			line.C1-1,
		),
	})
	m.Goto(geom.Vec2{
		R: line.R,
		C: geom.Clamp(
			line.C0+size.C/2,
			line.C0,
			line.C1-1,
		),
	})
}
