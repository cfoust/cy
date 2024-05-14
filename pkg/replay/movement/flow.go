package movement

import (
	"fmt"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/charmbracelet/lipgloss"
)

type flowMovement struct {
	emu.Terminal

	render *taro.Renderer

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
	f := &flowMovement{
		Terminal: terminal,
		render:   taro.NewRenderer(),
	}
	f.root = f.Root()
	f.viewport = viewport
	f.centerTerminalCursor()
	return f
}

// For a given location and viewport, find the first root that reveals the
// location's cell.
func (f *flowMovement) getRoot(viewport, location geom.Vec2) (root geom.Vec2, ok bool) {
	if location.LT(geom.Vec2{}) {
		location = geom.Vec2{}
		ok = true
		return
	}

	lastRoot := f.getLastRoot()
	if location.GTE(lastRoot) {
		location = lastRoot
		ok = true
		return
	}

	root = geom.Vec2{R: location.R}
	for {
		result := f.Flow(viewport, root)

		if !result.OK || len(result.Lines) == 0 {
			return
		}

		for _, line := range result.Lines {
			if location.C >= line.C0 && location.C < line.C1 {
				root = line.Root()
				ok = true
				return
			}
		}

		root = result.Lines[len(result.Lines)-1].Root()
	}
}

func (f *flowMovement) centerTerminalCursor() {
	// First just flow the viewport; if the whole screen fits, do
	// nothing
	result := f.Flow(f.viewport, f.root)
	if result.CursorOK {
		f.cursor = result.Cursor.Vec2
		f.desiredCol = f.cursor.C
		return
	}

	// Flow the screen no matter how big it is
	// By definition, cursor must be OK (we flow all lines)
	result = f.Flow(geom.Vec2{C: f.viewport.C}, f.root)
	if len(result.Lines) == 0 {
		return
	}

	// Move the viewport down just enough to reveal the cursor
	topIndex := geom.Clamp(
		result.Cursor.R-f.viewport.R+1,
		0,
		len(result.Lines)-1,
	)
	topLine := result.Lines[topIndex]
	f.root = topLine.Root()
	f.cursor.R = result.Cursor.R - topIndex
	f.cursor.C = result.Cursor.C
	f.desiredCol = f.cursor.C
}

func (f *flowMovement) ScrollTop() {
	f.haveMoved = true
	f.scrollToLine(geom.Vec2{R: 0, C: 0}, ScrollPositionTop)
	f.cursor.C = f.resolveScreenColumn(f.cursor.R)
}

func (f *flowMovement) ScrollBottom() {
	f.haveMoved = true
	f.scrollToLine(f.getLastRoot(), ScrollPositionBottom)
	f.cursor.C = f.resolveScreenColumn(f.cursor.R)
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

	if numLines == 0 {
		return
	}

	// Find the new root
	target := 0
	if !isUp {
		lastLine := f.getLastLine()

		// Ensure that we can't scroll past the last physical line
		for i := numLines - 1; i >= 0; i-- {
			target = i
			if result.Lines[i].Root().R <= lastLine {
				break
			}
		}
	}

	targetLine := result.Lines[target]
	f.root = targetLine.Root()

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

// getLastRoot returns the last root representing the upper limit for the
// scrollable region the user can reach. Mostly this is the last physical line
// on the screen; it's used primarily to prevent the user from scrolling onto
// blank lines at the end of the terminal screen.
func (f *flowMovement) getLastRoot() (lastRoot geom.Vec2) {
	screen := f.Flow(f.Terminal.Size(), f.Root())
	if len(screen.Lines) == 0 {
		return
	}

	// Return the row of the last non-empty physical line
	for row := len(screen.Lines) - 1; row >= 0; row-- {
		if !isLineEmpty(screen.Lines[row].Chars) {
			return screen.Lines[row].Root()
		}
	}

	return
}

func (f *flowMovement) getLastLine() int {
	return f.getLastRoot().R
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

	if oldSize == newSize {
		return
	}

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
	start, end = normalizeRange(start, end)

	numLines := end.R - start.R + 1
	lines := f.GetLines(start.R, end.R)
	if len(lines) != numLines {
		return
	}

	if start.R == end.R {
		return lines[0][start.C : end.C+1].String()
	}

	result += lines[0][start.C:].String() + "\n"

	for i := 1; i < len(lines)-1; i++ {
		result += lines[i].String() + "\n"
	}

	result += lines[len(lines)-1][:end.C+1].String()

	return result
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

	// TODO(cfoust): 04/01/24 how can this happen?
	if len(flow.Lines) == 0 {
		return
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
		return
	}

	position := ScrollPositionBottom
	if delta < 0 {
		position = ScrollPositionTop
	}

	f.scrollToLine(destLine.Root(), position)
}

func (f *flowMovement) Jump(needle string, isForward bool, isTo bool) {
	line, ok := f.getLine(f.cursor.R)
	if !ok {
		return
	}

	oldCol := f.cursor.C
	newCol := calculateJump(
		line.Chars,
		needle,
		isForward,
		isTo,
		oldCol,
	)
	f.MoveCursorX(newCol - oldCol)
}

func (f *flowMovement) highlightRow(
	row emu.Line,
	start, end geom.Vec2,
	screenLine emu.ScreenLine,
	highlight Highlight,
) {
	var (
		from = highlight.From
		to   = highlight.To
	)
	from, to = normalizeRange(from, to)

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

func (f *flowMovement) View(state *tty.State, highlights []Highlight) {
	r := f.render

	flow := f.Flow(f.viewport, f.root)
	screen := f.Flow(f.Terminal.Size(), f.Root())
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

		from, to = normalizeRange(from, to)
		highlight.From = from
		highlight.To = to
		highlight.Screen = false
		highlights[i] = highlight
	}

	image := state.Image
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

	termCursor := flow.Cursor
	if flow.CursorOK && f.cursor == termCursor.Vec2 {
		state.Cursor = termCursor
	} else {
		state.Cursor.Vec2 = f.cursor

		if flow.CursorOK {
			image[termCursor.R][termCursor.C].BG = 8
		}
	}

	if f.root.R >= f.Root().R {
		return
	}

	// Renders "[1/N]" text in the top-right corner that looks just like
	// tmux's copy mode, but works on physical lines instead.
	size := state.Image.Size()
	offsetStyle := f.render.NewStyle().
		Foreground(lipgloss.Color("9")).
		Background(lipgloss.Color("240"))

	r.RenderAt(
		state.Image,
		0,
		0,
		r.PlaceHorizontal(
			size.C,
			lipgloss.Right,
			offsetStyle.Render(fmt.Sprintf(
				"[%d/%d]",
				f.root.R,
				flow.NumLines,
			)),
		),
	)
}

func PreviewFlow(
	terminal emu.Terminal,
	size, location geom.Vec2,
	highlights []Highlight,
) *tty.State {
	image := tty.New(size)
	flow := NewFlow(terminal, size).(*flowMovement)

	if root, ok := flow.getRoot(size, location); ok {
		flow.scrollToLine(root, ScrollPositionCenter)
	}

	flow.View(image, highlights)
	return image
}
