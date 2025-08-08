package flow

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/replay/movement"
	"github.com/cfoust/cy/pkg/taro"
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

var _ movement.Movement = (*flowMovement)(nil)

func New(terminal emu.Terminal, viewport geom.Size) movement.Movement {
	f := &flowMovement{
		Terminal: terminal,
		render:   taro.NewRenderer(),
	}
	f.root = f.Root()
	f.viewport = viewport
	f.revealCursor()
	return f
}

// getRoot finds the root that contains the cell specified by `location`. Note
// that `location.C` must be the index of a cell _within_ a line.
func (f *flowMovement) getRoot(location geom.Vec2) (root geom.Vec2, ok bool) {
	// Ensure the location is valid
	lines := f.GetLines(location.R, location.R)
	if len(lines) != 1 {
		return
	}

	line := lines[0]
	if location.C < 0 || location.C > len(line) {
		return
	}

	root = geom.Vec2{R: location.R}
	for {
		result := f.Flow(f.viewport, root)

		if !result.OK || len(result.Lines) == 0 {
			return
		}

		for _, line := range result.Lines {
			if line.R != location.R {
				return
			}

			if location.C >= line.C0 && location.C <= line.C1 {
				root = line.Root()
				ok = true
				return
			}
		}

		root = result.Lines[len(result.Lines)-1].Root()
	}
}

func (f *flowMovement) Snap() {
	f.haveMoved = true
	f.cursor.C = f.resolveScreenColumn(f.cursor.R)
}

// revealCursor adjusts the root the minimum amount necessary to show the
// cursor before the user has moved.
func (f *flowMovement) revealCursor() {
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
	screen := f.Flow(f.Size(), f.Root())
	if len(screen.Lines) == 0 {
		return
	}

	// Return the row of the last non-empty physical line
	for row := len(screen.Lines) - 1; row >= 0; row-- {
		if !screen.Lines[row].Chars.IsEmpty() {
			return screen.Lines[row].Root()
		}
	}

	return
}

func (f *flowMovement) getLastLine() int {
	return f.getLastRoot().R
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
		f.revealCursor()
		return
	}

	root, ok := f.getRoot(cursor)
	if !ok {
		return
	}

	f.cursor.C = cursor.C - root.C
	f.desiredCol = f.cursor.C
	f.scrollToLine(root, ScrollPositionBottom)
}

func (f *flowMovement) MoveCursorX(delta int) {
	f.haveMoved = true

	current, ok := f.getLine(f.cursor.R)
	if !ok {
		return
	}

	_, lastCell := current.Chars.Whitespace()
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
