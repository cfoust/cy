package replay

import (
	"fmt"
	"time"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"

	"github.com/charmbracelet/lipgloss"
)

func (r *Replay) View(state *tty.State) {
	screen := r.terminal.Screen()
	history := r.terminal.History()
	size := state.Image.Size()
	state.CursorVisible = false

	// Return nothing when View() is called before we've actually gotten
	// the viewport
	if r.viewport.R == 0 && r.viewport.C == 0 {
		return
	}

	termSize := r.getTerminalSize()
	var point geom.Vec2
	var glyph emu.Glyph
	for row := 0; row < r.viewport.R; row++ {
		point.R = row + r.offset.R
		for col := 0; col < r.viewport.C; col++ {
			point.C = r.offset.C + col

			if point.C >= termSize.C || point.R >= termSize.R {
				glyph = emu.EmptyGlyph()
				glyph.FG = 8
				glyph.Char = '-'
			} else if point.R < 0 {
				glyph = history[len(history)+point.R][point.C]
			} else {
				glyph = screen[point.R][point.C]
			}

			state.Image[row][col] = glyph
		}
	}

	cursor := r.termToViewport(r.getTerminalCursor())
	if r.isSelectionMode {
		cursor = r.cursor
	} else {
		state.Cursor = r.terminal.Cursor()
	}
	state.Cursor.X = cursor.C
	state.Cursor.Y = cursor.R

	basic := r.render.NewStyle().
		Foreground(lipgloss.Color("#D5CCBA")).
		Background(lipgloss.Color("#000000")).
		Align(lipgloss.Right)

	index := r.index
	if index < 0 || index >= len(r.events) || len(r.events) == 0 {
		r.render.RenderAt(state, 0, 0, basic.Render("???"))
		return
	}

	headline := r.events[index].Stamp.Format(time.RFC1123)

	if r.offset.R < 0 {
		headline = fmt.Sprintf(
			"[%d/%d]",
			-r.offset.R,
			-r.minOffset.R,
		)
	}

	r.render.RenderAt(
		state,
		0,
		0,
		r.render.PlaceHorizontal(
			size.C,
			lipgloss.Right,
			basic.Render(headline),
		),
	)

	if !r.isSearching {
		return
	}

	r.render.RenderAt(
		state,
		geom.Clamp(r.terminal.Cursor().Y, 0, size.R-1),
		0,
		basic.Render(r.searchInput.View()),
	)

	state.CursorVisible = false
}
