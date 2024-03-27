package movement

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/rs/zerolog/log"
)

type flowMovement struct {
	emu.Terminal
	viewport geom.Size

	haveMoved bool

	// The location of the viewport in the history of the terminal's main
	// screen. See emu.Root().
	root geom.Vec2

	// The cursor's position relative to the viewport.
	cursor geom.Vec2

	// Used to mimic the behavior in text editors wherein moving the cursor
	// up and down "sticks" to a certain column index wherever possible
	desiredCol int
}

var _ Movement = (*flowMovement)(nil)

func NewFlow(terminal emu.Terminal, viewport geom.Size) Movement {
	f := &flowMovement{Terminal: terminal}
	f.root = f.Root()
	f.viewport = viewport
	f.centerTerminalCursor()
	return f
}

func (f *flowMovement) centerTerminalCursor() {
	// First just flow the viewport; if the whole screen fits, do
	// nothing
	result := f.Flow(f.viewport, f.root)
	if result.CursorOK {
		f.cursor = geom.Vec2{
			R: result.Cursor.Y,
			C: result.Cursor.X,
		}
		f.desiredCol = f.cursor.C
		return
	}

	// Flow the screen no matter how big it is
	// By definition, cursor must be OK (we flow all lines)
	result = f.Flow(geom.Vec2{C: f.viewport.C}, f.root)
	f.cursor.C = result.Cursor.X
	f.desiredCol = f.cursor.C
	f.scrollToLine(
		result.Lines[result.Cursor.Y].Root(),
		ScrollPositionCenter,
	)
}

func (f *flowMovement) ScrollTop() {
	f.haveMoved = true
	// TODO(cfoust): 03/25/24
}

func (f *flowMovement) ScrollBottom() {
	f.haveMoved = true
	// TODO(cfoust): 03/25/24
}

func (f *flowMovement) ScrollYDelta(delta int) {
	f.haveMoved = true

	isUp := delta < 0

	// Account for the fact that Flow() returns the root line as well
	if !isUp {
		delta++
	}

	result := f.Flow(geom.Vec2{
		C: f.viewport.C,
		R: delta,
	}, f.root)

	numLines := len(result.Lines)

	// Find the new root
	target := 0
	if !isUp {
		target = numLines - 1
	}

	targetLine := result.Lines[target]
	f.root = geom.Vec2{
		R: targetLine.R,
		C: targetLine.C0,
	}

	newRow := f.cursor.R
	if isUp {
		newRow += numLines
	} else {
		// -1 because we're skipping the root line
		newRow -= numLines - 1
	}

	newRow = geom.Clamp(newRow, 0, f.viewport.R-1)
	f.cursor = geom.Vec2{
		R: newRow,
		C: f.resolveScreenColumn(newRow),
	}
}

func (f *flowMovement) ScrollXDelta(delta int) {
	// no-op in this mode
}

// getLine gets a line on the screen in flow mode. Providing a negative
// `row` returns lines from history.
func (f *flowMovement) getLine(row int) (line emu.ScreenLine, ok bool) {
	if row >= 0 {
		// Include the root line
		row++
	}

	flow := f.Flow(geom.Vec2{
		R: row,
		C: f.viewport.C,
	}, f.root)
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

// getLastLine returns the last root representing the upper limit for the
// scrollable region the user can reach. Mostly this is the last physical line
// on the screen; it's used primarily to prevent the user from scrolling onto
// blank lines at the end of the terminal screen.
func (f *flowMovement) getLastLine() int {
	screen := f.Flow(getTerminalSize(f.Terminal), f.Root())
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

func (f *flowMovement) scrollToLine(dest geom.Vec2, position ScrollPosition) {
	if dest.R < 0 || dest.C < 0 {
		return
	}

	if dest.R > f.getLastLine() {
		return
	}

	// If the line is on the screen, we don't need to scroll
	viewport := f.Flow(f.viewport, f.root)
	for row, line := range viewport.Lines {
		if line.Root() != dest {
			continue
		}

		f.cursor.R = row
		break
	}

	var rows int
	switch position {
	case ScrollPositionCenter:
		rows = f.viewport.R / 2
	case ScrollPositionBottom:
		rows = f.viewport.R - 1
	}

	rows = geom.Max(rows, 0)

	if rows == 0 {
		f.root = dest
		f.cursor.R = 0
		return
	}

	flow := f.Flow(geom.Vec2{
		C: f.viewport.C,
		R: -1 * rows,
	}, dest)
	if !flow.OK {
		return
	}

	lines := flow.Lines
	f.root = lines[0].Root()
	f.cursor.R = len(lines)
}

// Given a point in term space representing a desired cursor position, return
// the best available cursor position. This enables behavior akin to moving up
// and down in a text editor.
func (f *flowMovement) resolveScreenColumn(row int) int {
	result := f.Flow(f.viewport, f.root)
	if !result.OK {
		return 0
	}

	lines := result.Lines
	if row < 0 || row >= len(lines) {
		return 0
	}

	return resolveDesiredColumn(lines[row].Chars, f.desiredCol)
}

func (f *flowMovement) Resize(newSize geom.Vec2) {
	cursor := f.Cursor()
	oldSize := f.viewport
	f.viewport = newSize

	// The normal terminal cursor can be anywhere, so we have to use the
	// built-in cursor reflow when we haven't moved yet. After moving,
	// movement is constrained to cells with printable characters.
	if !f.haveMoved {
		f.centerTerminalCursor()
		return
	}

	flow := f.Flow(
		geom.Vec2{
			C: newSize.C,
			R: (oldSize.C*oldSize.R)/newSize.C + 1,
		},
		f.root,
	)

	// By definition the cursor must be on the new screen
	var dest emu.ScreenLine
	for _, line := range flow.Lines {
		if cursor.R != line.R || cursor.C < line.C0 || cursor.C >= line.C1 {
			continue
		}

		log.Info().Msgf("%+v %+v %+v", line, cursor, f.cursor)
		dest = line
		f.cursor.C = cursor.C - line.C0
		f.desiredCol = f.cursor.C
		break
	}

	f.scrollToLine(dest.Root(), ScrollPositionCenter)
}

func (f *flowMovement) Cursor() geom.Vec2 {
	result := f.Flow(f.viewport, f.root)

	for row, line := range result.Lines {
		if f.cursor.R != row {
			continue
		}

		numChars := line.C1 - line.C0
		cursor := geom.Vec2{
			R: line.R,
			C: line.C0 + f.cursor.C,
		}

		// We need to return the address of a real cell
		if f.cursor.C >= numChars {
			_, lastCell := getNonWhitespace(line.Chars)
			cursor.C = lastCell
		}

		return cursor
	}

	return geom.Vec2{}
}

func (f *flowMovement) ReadString(start, end geom.Vec2) (result string) {
	return ""
}

func (f *flowMovement) MoveCursorX(delta int) {
	f.haveMoved = true

	current, ok := f.getLine(f.cursor.R)
	if !ok {
		return
	}

	_, lastCell := getNonWhitespace(current.Chars)
	oldCol := f.cursor.C
	newCol := geom.Clamp(
		f.cursor.C+delta,
		0,
		lastCell,
	)

	// Don't do anything if we can't move
	if newCol == oldCol {
		return
	}

	f.cursor.C = newCol
	f.desiredCol = newCol
}

func (f *flowMovement) MoveCursorY(delta int) {
	f.haveMoved = true

	current, ok := f.getLine(f.cursor.R)
	if !ok {
		return
	}

	numRows := delta
	if delta >= 0 {
		// Include the root line
		numRows++
	}

	// We want to flow from the current line of the cursor to its
	// destination so that we can determine how much to move the viewport
	// and where to leave the cursor.
	flow := f.Flow(geom.Vec2{
		R: numRows,
		C: f.viewport.C,
	}, geom.Vec2{
		R: current.R,
		C: current.C0,
	})
	if !flow.OK {
		return
	}

	// Ensure the user can't move past the last physical line
	lastLine := f.getLastLine()
	for i := 0; i < len(flow.Lines); i++ {
		if flow.Lines[i].Root().R <= lastLine {
			continue
		}

		flow.Lines = flow.Lines[:i]
		break
	}

	destLine := flow.Lines[0]
	if delta >= 0 {
		destLine = flow.Lines[len(flow.Lines)-1]
	}

	if destLine.Root() == current.Root() {
		return
	}

	f.cursor.C = resolveDesiredColumn(destLine.Chars, f.desiredCol)

	// If the line is on the screen, we don't need to scroll
	viewport := f.Flow(f.viewport, f.root)
	for row, line := range viewport.Lines {
		if line.Root() != destLine.Root() {
			continue
		}

		f.cursor.R = row
		break
	}

	position := ScrollPositionBottom
	if delta < 0 {
		position = ScrollPositionTop
	}

	f.scrollToLine(destLine.Root(), position)
}

func (f *flowMovement) Jump(needle string, isForward bool, isTo bool) {
	// TODO(cfoust): 03/25/24
}
