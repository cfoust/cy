package thumbs

import (
	"strings"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
)

func (t *Thumbs) renderMatch(
	state *tty.State,
	input string,
	hint string,
	match Match,
) {
	var (
		size       = state.Image.Size()
		isBlank    = len(input) == 0
		doesMatch  = strings.HasPrefix(hint, input)
		background = emu.Yellow
		hintRunes  = []rune(hint)
	)

	if !isBlank {
		if doesMatch {
			background = emu.LightBlue
		} else {
			background = emu.LightGrey
		}
	}

	for i, cell := range match {
		if cell.R >= size.R || cell.C >= size.C {
			continue
		}

		glyph := state.Image[cell.R][cell.C]
		glyph.BG = background

		if i < len(hintRunes) {
			glyph.Char = hintRunes[i]
			glyph.Mode = emu.AttrBold
		}

		state.Image[cell.R][cell.C] = glyph
	}
}

func (t *Thumbs) View(state *tty.State) {
	var (
		size  = state.Image.Size()
		input = t.textInput.Value()
	)

	state.CursorVisible = false

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

	for hint, match := range t.hints {
		t.renderMatch(state, input, hint, match)
	}
}
