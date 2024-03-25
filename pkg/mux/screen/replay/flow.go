package replay

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
)

// getFlowLine gets a line on the screen in flow mode. Providing a negative
// `row` returns lines from history.
func (r *Replay) getFlowLine(row int) (line emu.ScreenLine, ok bool) {
	if row >= 0 {
		// Include the root line
		row++
	}

	flow := r.Flow(geom.Vec2{
		R: row,
		C: r.viewport.C,
	}, r.root)
	if !flow.OK {
		return
	}

	if len(flow.Lines) < geom.Abs(row) {
		return
	}

	if row < 0 {
		line = flow.Lines[0]
	} else {
		line = flow.Lines[row-1]
	}

	ok = true
	return
}

// getLastFlow returns the last root representing the upper limit for the
// scrollable region the user can reach. Mostly this is the last physical line
// on the screen; it's used primarily to prevent the user from scrolling onto
// blank lines at the end of the terminal screen.
func (r *Replay) getLastFlow() int {
	screen := r.Flow(r.getTerminalSize(), r.Root())
	if len(screen.Lines) == 0 {
		return 0
	}

	// Return the row of the last non-empty physical line
	for row := len(screen.Lines) - 1; row >= 0; row-- {
		if !isLineEmpty(screen.Lines[row].Chars) {
			return screen.Lines[row].Root().R
		}
	}

	return 0
}

type ScrollPosition int

const (
	ScrollPositionTop ScrollPosition = iota
	ScrollPositionCenter
	ScrollPositionBottom
)

func (r *Replay) scrollToFlowLine(dest geom.Vec2, position ScrollPosition) {
	if dest.R < 0 || dest.C < 0 {
		return
	}

	if dest.R > r.getLastFlow() {
		return
	}

	// If the line is on the screen, we don't need to scroll
	viewport := r.Flow(r.viewport, r.root)
	for row, line := range viewport.Lines {
		if line.Root() != dest {
			continue
		}

		r.cursor.R = row
		break
	}

	var rows int
	switch position {
	case ScrollPositionCenter:
		rows = r.viewport.R/2 - 1
	case ScrollPositionBottom:
		rows = r.viewport.R - 1
	}

	rows = geom.Max(rows, 0)

	if rows == 0 {
		r.root = dest
		r.cursor.R = 0
		return
	}

	flow := r.Flow(geom.Vec2{
		C: r.viewport.C,
		R: -1 * rows,
	}, dest)
	if !flow.OK {
		return
	}

	lines := flow.Lines
	r.root = lines[0].Root()
	r.cursor.R = len(lines)
}
