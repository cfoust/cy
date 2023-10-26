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
		from := r.termToViewport(match.From)
		to := r.termToViewport(match.To)
		if !r.isInViewport(from) || !r.isInViewport(to) {
			continue
		}

		for row := from.R; row <= to.R; row++ {
			for col := from.C; col < to.C; col++ {
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
		Background(lipgloss.Color("8"))

	statusText := "⏵"
	statusBG := lipgloss.Color("6")
	if r.isSelectionMode {
		statusText = "SELECT"
		statusBG = lipgloss.Color("3")
	}
	if r.isPlaying {
		statusText = "⏸"
		statusBG = lipgloss.Color("5")
	}

	if !r.isSelectionMode && r.playbackRate != 1 {
		statusText += fmt.Sprintf(" %dx", r.playbackRate)
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

	if r.offset.R < 0 {
		offsetStyle := r.render.NewStyle().
			Foreground(lipgloss.Color("9")).
			Background(lipgloss.Color("240"))

		linePos := r.termToViewport(geom.Vec2{R: 0}).R
		if linePos >= 0 && linePos < r.viewport.R {
			r.render.RenderAt(
				state,
				linePos,
				r.viewport.C-3,
				offsetStyle.Render("<--"),
			)
		}

		r.render.RenderAt(
			state,
			0,
			0,
			r.render.PlaceHorizontal(
				size.C,
				lipgloss.Right,
				offsetStyle.Render(fmt.Sprintf(
					"[%d/%d]",
					-r.offset.R,
					-r.minOffset.R,
				)),
			),
		)
	}

	statusBar := statusBarStyle.
		Width(size.C).
		Height(1).
		Render(lipgloss.JoinHorizontal(lipgloss.Top,
			statusStyle.Render(statusText),
			statusBarStyle.Render(r.currentTime.Format(time.RFC1123)),
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

	termCursor := r.termToViewport(r.getTerminalCursor())
	if r.isSelectionMode {
		state.Cursor.X = r.cursor.C
		state.Cursor.Y = r.cursor.R

		// In selection mode, leave behind a ghost cursor where the
		// terminal's cursor is
		if r.isInViewport(termCursor) {
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

	// Layer 4: Render text input
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
