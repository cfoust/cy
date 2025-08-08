package thumbs

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
)

func (t *Thumbs) View(state *tty.State) {
	size := state.Image.Size()

	if size.IsZero() {
		return
	}

	image.Copy(geom.Vec2{}, state.Image, t.initial)

	for row := range size.R {
		for col := range size.C {
			state.Image[row][col].FG = emu.DefaultFG
			state.Image[row][col].BG = emu.DefaultBG
		}
	}

	for _, match := range t.hints {
		for _, cell := range match {
			if cell.R >= size.R || cell.C >= size.C {
				continue
			}

			state.Image[cell.R][cell.C].FG = emu.Yellow
		}
	}
}
