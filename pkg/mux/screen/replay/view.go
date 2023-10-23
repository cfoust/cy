package replay

import (
	"fmt"
	"time"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"

	"github.com/charmbracelet/lipgloss"
)

func (r *Replay) drawMatches(state *tty.State) {
	matches := r.matches
	if len(matches) == 0 {
		return
	}

	location := r.location
	for i, match := range matches {
		// This match is not on the screen
		if location.Compare(match.Begin) < 0 || location.Compare(match.End) >= 0 {
			continue
		}

		isSelected := i == r.matchIndex && location.Compare(match.Begin) == 0

		for row := match.From.R; row <= match.To.R; row++ {
			for col := match.From.C; col <= match.To.C; col++ {
				state.Image[row][col].FG = 1

				var bg emu.Color = 14
				if isSelected {
					bg = 13
				}
				state.Image[row][col].BG = bg
			}
		}
	}
}

func (r *Replay) drawStatusBar(state *tty.State) {
	size := state.Image.Size()

	statusBarStyle := r.render.NewStyle().
		Foreground(lipgloss.Color("0")).
		Background(lipgloss.Color("251"))

	statusText := "TIME"
	statusBG := lipgloss.Color("6")
	if r.isSelectionMode {
		statusText = "SELECT"
		statusBG = lipgloss.Color("3")
	}

	statusStyle := r.render.NewStyle().
		Inherit(statusBarStyle).
		Background(statusBG).
		Padding(0, 1).
		MarginRight(1)

	index := r.location.Index
	if index < 0 || index >= len(r.events) || len(r.events) == 0 {
		return
	}

	timestamp := r.events[index].Stamp.Format(time.RFC1123)

	if r.offset.R < 0 {
		timestamp = fmt.Sprintf(
			"[%d/%d]",
			-r.offset.R,
			-r.minOffset.R,
		)
	}

	statusBar := statusBarStyle.Width(size.C).Height(1).Render(lipgloss.JoinHorizontal(lipgloss.Top,
		statusStyle.Render(statusText),
		statusBarStyle.Render(timestamp),
	))

	r.render.RenderAt(state, size.R-1, 0, statusBar)
}

func (r *Replay) View(state *tty.State) {
	screen := r.terminal.Screen()
	history := r.terminal.History()
	state.CursorVisible = true

	// Return nothing when View() is called before we've actually gotten
	// the viewport
	if r.viewport.R == 0 && r.viewport.C == 0 {
		return
	}

	// Layer 1: Draw the underlying terminal state
	//////////////////////////////////////////////
	termSize := r.getTerminalSize()
	var point geom.Vec2
	var glyph emu.Glyph
	for row := 0; row <= r.viewport.R; row++ {
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

	viewport := r.viewport
	termCursor := r.termToViewport(r.getTerminalCursor())
	if r.isSelectionMode {
		state.Cursor.X = r.cursor.C
		state.Cursor.Y = r.cursor.R

		// In selection mode, leave behind a ghost cursor where the
		// terminal's cursor is
		if termCursor != r.cursor && termCursor.R >= 0 && termCursor.R < viewport.R && termCursor.C >= 0 && termCursor.C < viewport.C {
			state.Image[termCursor.R][termCursor.C].BG = 8
		}
	} else {
		state.Cursor = r.terminal.Cursor()
		state.Cursor.X = termCursor.C
		state.Cursor.Y = termCursor.R
	}

	// Layer 2: Highlight any matches on the screen
	///////////////////////////////////////////////
	r.drawMatches(state)

	// Layer 3: Render overlays
	///////////////////////////
	r.drawStatusBar(state)

	// Layer 3: Render text input
	/////////////////////////////
	if !r.isSearching {
		return
	}

	r.searchInput.Cursor.Style = r.render.NewStyle().
		Background(lipgloss.Color("#EAA549"))

	// hide the cursor when typing in the search bar (it has its own)
	state.CursorVisible = false
	r.render.RenderAt(
		state,
		r.cursor.R,
		r.cursor.C,
		r.searchInput.View(),
	)
}
