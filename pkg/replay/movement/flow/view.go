package flow

import (
	"fmt"
	"math"

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

	if highlight.Selection == movement.SelectLine {
		from.C = 0
		to.C = math.MaxInt32
	}

	// Row must be in range
	if from.R > start.R || to.R < start.R {
		return
	}

	var startCol, endCol int

	if highlight.Selection == movement.SelectBlock {
		// Block (rectangular) selection: use the column
		// bounds on every row
		startCol = geom.Min(from.C, to.C) - screenLine.C0
		endCol = geom.Max(from.C, to.C) - screenLine.C0
	} else {
		//-e |     |
		//   |     | s-
		if from.GTE(end) || to.LT(start) {
			return
		}

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
		if highlight.Style != nil {
			highlight.Style.Apply(&row[col])
		}
	}
}

func (f *flowMovement) View(
	params *params.Parameters,
	state *tty.State,
	highlights []movement.Highlight,
) {
	r := f.render

	screenRoot := f.Root()

	flow := f.Flow(f.viewport, f.root)
	screen := f.Flow(f.Size(), screenRoot)
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

	if f.root.R >= screenRoot.R {
		return
	}

	// Renders "[1/N]" text in the top-right corner that looks just like
	// tmux's copy mode, but works on physical lines instead.
	offsetStyle := params.ReplaySelectionStyle().Style

	firstLine := 0
	if f.IsAltMode() {
		firstLine = f.getFirstLine()
	} else {
		history := f.History()
		firstLine = screenRoot.R - len(history)
		if len(history) > 0 && history[len(history)-1].IsWrapped() {
			firstLine++
		}
	}

	num := screenRoot.R - f.root.R
	den := screenRoot.R - firstLine
	if den < 1 {
		den = 1
	}
	num = geom.Clamp(num, 0, den)

	suffix := ""
	if firstLine > 0 {
		suffix = "?"
	}

	r.RenderAt(
		state.Image,
		0,
		0,
		r.PlaceHorizontal(
			size.C,
			lipgloss.Right,
			offsetStyle.Render(fmt.Sprintf(
				"[%d/%d%s]",
				num,
				den,
				suffix,
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
