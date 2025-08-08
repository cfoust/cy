package flow

import (
	"fmt"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/params"
	"github.com/cfoust/cy/pkg/replay/movement"
	"github.com/cfoust/cy/pkg/style"

	"github.com/charmbracelet/lipgloss"
)

func (f *flowMovement) highlightRow(
	row emu.Line,
	start, end geom.Vec2,
	screenLine emu.ScreenLine,
	highlight movement.Highlight,
) {
	var (
		from = highlight.From
		to   = highlight.To
	)
	from, to = geom.NormalizeRange(from, to)

	//-e |     |
	//   |     | s-
	if from.GTE(end) || to.LT(start) {
		return
	}

	var startCol, endCol int

	//   |  s--|-
	if from.LT(end) && from.GTE(start) {
		startCol = from.C - screenLine.C0
	}

	//  -|--e  |
	if to.GTE(start) || to.LT(end) {
		endCol = to.C - screenLine.C0
	}

	//   |-----|-e
	if to.GTE(end) {
		endCol = len(row) - 1
	}

	// Flow does not highlight past the last non-whitespace cell in the
	// line
	endCol = geom.Min(endCol, screenLine.C1-screenLine.C0-1)

	// Also bound this by the end of the line as a safety measure
	endCol = geom.Clamp(endCol, 0, len(row)-1)

	if startCol > endCol {
		return
	}

	for col := startCol; col <= endCol; col++ {
		row[col].FG = highlight.FG
		row[col].BG = highlight.BG
	}
}

func (f *flowMovement) View(
	params *params.Parameters,
	state *tty.State,
	highlights []movement.Highlight,
) {
	r := f.render

	flow := f.Flow(f.viewport, f.root)
	screen := f.Flow(f.Size(), f.Root())
	if !flow.OK || !screen.OK {
		return
	}

	// Transform all highlights into physical line coordinates
	for i, highlight := range highlights {
		if !highlight.Screen {
			continue
		}

		from, fromOK := screen.Coord(highlight.From)
		to, toOK := screen.Coord(highlight.To)
		if !fromOK || !toOK {
			continue
		}

		from, to = geom.NormalizeRange(from, to)
		highlight.From = from
		highlight.To = to
		highlight.Screen = false
		highlights[i] = highlight
	}

	image := state.Image
	size := state.Image.Size()
	termCursor := flow.Cursor
	if flow.CursorOK && f.cursor == termCursor.Vec2 {
		state.Cursor = termCursor
	} else {
		state.Cursor.Vec2 = f.cursor

		if flow.CursorOK {
			style.GhostCursor(
				state.Image,
				termCursor.R,
				termCursor.C,
			)
		}
	}

	var start, end geom.Vec2
	for row, line := range flow.Lines {
		copy(image[row], line.Chars)

		start = line.Root()
		end = geom.Vec2{R: line.R, C: line.C1}

		for _, highlight := range highlights {
			f.highlightRow(
				image[row],
				start, end,
				line,
				highlight,
			)
		}
	}

	if f.root.R >= f.Root().R {
		return
	}

	// Renders "[1/N]" text in the top-right corner that looks just like
	// tmux's copy mode, but works on physical lines instead.
	offsetStyle := f.render.NewStyle().
		Foreground(params.ReplaySelectionFg()).
		Background(params.ReplaySelectionBg())

	r.RenderAt(
		state.Image,
		0,
		0,
		r.PlaceHorizontal(
			size.C,
			lipgloss.Right,
			offsetStyle.Render(fmt.Sprintf(
				"[%d/%d]",
				f.Root().R-f.root.R,
				f.Root().R,
			)),
		),
	)
}

func PreviewFlow(
	params *params.Parameters,
	terminal emu.Terminal,
	size, location geom.Vec2,
	highlights []movement.Highlight,
) *tty.State {
	image := tty.New(size)
	flow := New(terminal, size).(*flowMovement)
	flow.Goto(location)
	flow.View(params, image, highlights)
	return image
}
