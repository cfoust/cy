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
		i         = state.Image
		size      = i.Size()
		numInput  = len(input)
		doesMatch = numInput > 0 && strings.HasPrefix(hint, input)
		hintRunes = []rune(hint)
	)

	for index, cell := range match {
		if cell.R >= size.R || cell.C >= size.C {
			continue
		}

		var (
			glyph     = i[cell.R][cell.C]
			isHint    = index < len(hintRunes)
			isCorrect = doesMatch && isHint && index < numInput
		)

		if isHint {
			glyph.Char = hintRunes[index]
			glyph.Mode = emu.AttrBold

			glyph.FG = emu.Yellow
			glyph.BG = emu.DefaultBG

			if isCorrect {
				glyph.FG = emu.White
				glyph.BG = emu.Blue
			}
		} else {
			glyph.FG = emu.Red
			glyph.BG = emu.DefaultBG
		}

		i[cell.R][cell.C] = glyph
	}
}

func (t *Thumbs) View(state *tty.State) {
	var (
		i     = state.Image
		size  = i.Size()
		input = t.textInput.Value()
	)

	state.CursorVisible = false

	if size.IsZero() {
		return
	}

	t.lines.Prepare(size)
	t.lines.SetTarget(i)

	image.Copy(geom.Vec2{}, i, t.initial)

	for hint, match := range t.hints {
		t.renderMatch(state, input, hint, match)
	}
}
