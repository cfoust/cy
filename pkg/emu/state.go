package emu

import (
	"io"
	"log"

	"github.com/cfoust/cy/pkg/geom"

	"github.com/danielgatis/go-vte/vtparser"
	"github.com/mattn/go-runewidth"
	"github.com/sasha-s/go-deadlock"
)

const (
	tabspaces = 8
)

const (
	attrReverse = 1 << iota
	attrUnderline
	attrBold
	attrGfx
	attrItalic
	attrBlink
	attrWrap
	attrBlank
)

// State represents the terminal emulation state. Use Lock/Unlock
// methods to synchronize data access with VT.
type State struct {
	deadlock.RWMutex
	DebugLogger *log.Logger

	w          io.Writer
	cols, rows int

	screen, altScreen   []Line
	history, altHistory []Line
	// Whether the last cell of history _continues to wrap onto the screen_.
	// This is only used when `disableHistory` is true, since we still need
	// the wrapping behavior to be correct.
	wrapped, altWrapped bool

	cur, curSaved Cursor
	top, bottom   int // scroll limits
	mode          ModeFlag
	str           strEscape
	csi           csiEscape
	tabs          []bool
	title         string
	colorOverride map[Color]Color

	dirty *Dirty

	// whether scrolling up should send lines to the scrollback buffer
	disableHistory bool

	parser *vtparser.Parser
}

func newState(w io.Writer) *State {
	t := &State{
		w:             w,
		colorOverride: make(map[Color]Color),
		dirty: &Dirty{
			hooks:     make(map[string]bool),
			hookState: make([]byte, 256),
		},
	}

	t.parser = vtparser.New(
		t.Print,
		t.Execute,
		t.Put,
		t.Unhook,
		t.Hook,
		t.OscDispatch,
		t.CsiDispatch,
		t.EscDispatch,
	)

	return t
}

func (t *State) logf(format string, args ...interface{}) {
	if t.DebugLogger != nil {
		t.DebugLogger.Printf(format, args...)
	}
}

func (t *State) logln(s string) {
	if t.DebugLogger != nil {
		t.DebugLogger.Println(s)
	}
}

// Cell returns the glyph containing the character code, foreground color, and
// background color at position (x, y) relative to the top left of the terminal.
// TODO(cfoust): 08/24/23 move this to row, col
func (t *State) Cell(x, y int) Glyph {
	cell := t.screen[y][x]
	fg, ok := t.colorOverride[cell.FG]
	if ok {
		cell.FG = fg
	}
	bg, ok := t.colorOverride[cell.BG]
	if ok {
		cell.BG = bg
	}
	return cell
}

// Cursor returns the current position of the cursor.
func (t *State) Cursor() Cursor {
	t.RLock()
	defer t.RUnlock()
	return t.cur
}

// CursorVisible returns the visible state of the cursor.
func (t *State) CursorVisible() bool {
	t.RLock()
	defer t.RUnlock()
	return t.mode&ModeHide == 0
}

// Mode returns the current terminal mode.
func (t *State) Mode() ModeFlag {
	t.RLock()
	defer t.RUnlock()
	return t.mode
}

// Title returns the current title set via the tty.
func (t *State) Title() string {
	t.RLock()
	defer t.RUnlock()
	return t.title
}

/*
// ChangeMask returns a bitfield of changes that have occured by VT.
func (t *State) ChangeMask() ChangeFlag {
	return t.changed
}
*/

func (t *State) saveCursor() {
	t.curSaved = t.cur
}

// Restore the cursor, retaining its state if `keepState` is true.
func (t *State) restoreCursor(keepState bool) {
	t.cur = t.curSaved
	state := t.curSaved.State
	t.moveTo(t.cur.C, t.cur.R)
	if !keepState {
		return
	}
	t.cur.State = state
}

func (t *State) putTab(forward bool) {
	x := t.cur.C
	if forward {
		if x == t.cols {
			return
		}
		for x++; x < t.cols && !t.tabs[x]; x++ {
		}
	} else {
		if x == 0 {
			return
		}
		for x--; x > 0 && !t.tabs[x]; x-- {
		}
	}
	t.moveTo(x, t.cur.R)
}

func (t *State) newline(firstCol bool) {
	y := t.cur.R
	if y == t.bottom {
		cur := t.cur
		t.cur = t.defaultCursor()
		t.scrollUp(t.top, 1)
		t.cur = cur
	} else {
		y++
	}
	if firstCol {
		t.moveTo(0, y)
	} else {
		t.moveTo(t.cur.C, y)
	}
}

// table from st, which in turn is from rxvt :)
var gfxCharTable = [62]rune{
	'↑', '↓', '→', '←', '█', '▚', '☃', // A - G
	0, 0, 0, 0, 0, 0, 0, 0, // H - O
	0, 0, 0, 0, 0, 0, 0, 0, // P - W
	0, 0, 0, 0, 0, 0, 0, ' ', // X - _
	'◆', '▒', '␉', '␌', '␍', '␊', '°', '±', // ` - g
	'␤', '␋', '┘', '┐', '┌', '└', '┼', '⎺', // h - o
	'⎻', '─', '⎼', '⎽', '├', '┤', '┴', '┬', // p - w
	'│', '≤', '≥', 'π', '≠', '£', '·', // x - ~
}

func (t *State) setChar(c rune, attr *Glyph, x, y int) {
	w := runewidth.RuneWidth(c)

	if attr.Mode&attrGfx != 0 {
		if c >= 0x41 && c <= 0x7e && gfxCharTable[c-0x41] != 0 {
			c = gfxCharTable[c-0x41]
		}
	}

	t.dirty.markScreen()
	t.markDirtyLine(y)

	t.dirty.Printed = false
	t.dirty.Print.R = y
	t.dirty.Print.C = x

	for i := x; i < len(t.screen[y]) && i < x+w; i++ {
		t.screen[y][i] = *attr
		// Every explicit character change means cell is no longer
		// blank (important for wrapping)
		t.screen[y][i].Mode &= ^attrBlank
		t.screen[y][i].Write = t.dirty.writeId
		// super useful for debugging
		//t.screen[y][i].BG = Color(t.dirty.writeId % 255)

		if i == x {
			t.screen[y][i].Char = c
			t.dirty.Print.Glyph = t.screen[y][i]
			t.dirty.Printed = true
		} else {
			t.screen[y][i].Char = ' '
		}
		//if t.options.BrightBold && attr.Mode&attrBold != 0 && attr.FG < 8 {
		if attr.Mode&attrBold != 0 && attr.FG < 8 {
			t.screen[y][i].FG = attr.FG + 8
		}
		if attr.Mode&attrReverse != 0 {
			t.screen[y][i].FG = attr.BG
			t.screen[y][i].BG = attr.FG
		}
	}
}

func (t *State) defaultCursor() Cursor {
	c := Cursor{}
	c.Attr.FG = DefaultFG
	c.Attr.BG = DefaultBG
	return c
}

func (t *State) reset() {
	t.cur = t.defaultCursor()
	t.saveCursor()
	for i := range t.tabs {
		t.tabs[i] = false
	}
	for i := tabspaces; i < len(t.tabs); i += tabspaces {
		t.tabs[i] = true
	}
	t.top = 0
	t.bottom = t.rows - 1
	t.mode = ModeWrap
	t.clear(0, 0, t.rows-1, t.cols-1)
	t.moveTo(0, 0)
}

// TODO: definitely can improve allocs
func (t *State) resize(size geom.Vec2) {
	oldCols := t.cols
	cols := size.C
	rows := size.R
	if cols == t.cols && rows == t.rows {
		return
	}
	if cols < 1 || rows < 1 {
		return
	}

	// Get rid of any wrapped lines (kitty does this too)
	// TODO(cfoust): 02/28/24 what about in the alt screen?
	for isWrappedLines(t.history) || (t.disableHistory && t.wrapped) {
		t.scrollUp(0, 1)
	}

	tabs := t.tabs
	screen, altScreen := t.screen, t.altScreen
	history, altHistory := t.history, t.altHistory
	t.screen = make([]Line, rows)
	t.altScreen = make([]Line, rows)
	t.dirty.Lines = make(map[int]bool, rows)
	t.tabs = make([]bool, cols)

	t.dirty.markScreen()
	for i := 0; i < rows; i++ {
		t.markDirtyLine(i)
		t.screen[i] = emptyLine(cols)
		t.altScreen[i] = emptyLine(cols)
	}

	t.cols = cols
	t.rows = rows
	if cols > 0 && rows > 0 {
		t.clear(0, 0, cols-1, rows-1)
	}

	// Reflow lines and send lines to history when necessary
	{
		var (
			oldScreen  = screen
			newScreen  = t.screen
			oldCursor  = t.cur
			newHistory = history
		)
		wrapped := false
		if IsAltMode(t.mode) {
			oldScreen = altScreen
			newScreen = t.altScreen
			oldCursor = t.curSaved
			newHistory = altHistory
		}

		newLines, newCursor, cursorValid := reflow(oldScreen, oldCursor, cols)

		numExtra := max(len(newLines)-rows, 0)
		if numExtra > 0 {
			for i := range newLines[:numExtra] {
				wrapped = newLines[i].IsWrapped()
				if t.disableHistory {
					continue
				}
				newHistory = appendWrapped(
					newHistory,
					newLines[i],
				)
			}
		}

		if IsAltMode(t.mode) {
			t.altHistory = newHistory
		} else {
			t.history = newHistory
		}

		if t.disableHistory {
			if IsAltMode(t.mode) {
				t.altWrapped = wrapped
			} else {
				t.wrapped = wrapped
			}
		}

		// Only respect the cursor position we received if there
		// actually were lines involved
		if cursorValid {
			newCursor.R -= numExtra
			newCursor.C = clamp(newCursor.C, 0, cols-1)
			newCursor.R = clamp(newCursor.R, 0, rows-1)
			if !IsAltMode(t.mode) {
				t.cur = newCursor
			} else {
				t.curSaved = newCursor
			}
		}

		for i, line := range newLines[numExtra:] {
			copy(newScreen[i], line)
		}

	}

	copy(t.tabs, tabs)
	if cols > oldCols && oldCols > 0 {
		i := oldCols - 1
		for i > 0 && !tabs[i] {
			i--
		}
		for i += tabspaces; i < len(t.tabs); i += tabspaces {
			t.tabs[i] = true
		}
	}

	t.setScroll(0, rows-1)
	t.moveTo(t.cur.C, t.cur.R)
}

func (t *State) clear(x0, y0, x1, y1 int) {
	if x0 > x1 {
		x0, x1 = x1, x0
	}
	if y0 > y1 {
		y0, y1 = y1, y0
	}
	x0 = clamp(x0, 0, t.cols-1)
	x1 = clamp(x1, 0, t.cols-1)
	y0 = clamp(y0, 0, t.rows-1)
	y1 = clamp(y1, 0, t.rows-1)
	t.dirty.markScreen()
	for y := y0; y <= y1; y++ {
		t.markDirtyLine(y)
		for x := x0; x <= x1; x++ {
			t.screen[y][x] = t.cur.Attr
			t.screen[y][x].Char = ' '
			t.screen[y][x].Write = t.dirty.writeId
			t.screen[y][x].Mode |= attrBlank
		}
	}

	t.dirty.Cleared = true
	t.dirty.Clear = geom.Rect{
		Position: geom.Vec2{
			R: y0,
			C: x0,
		},
		Size: geom.Vec2{
			R: y1 - y0 + 1,
			C: x1 - x0 + 1,
		},
	}
}

func (t *State) clearAll() {
	t.clear(0, 0, t.cols-1, t.rows-1)
}

func (t *State) moveAbsTo(x, y int) {
	if t.cur.State&cursorOrigin != 0 {
		y += t.top
	}
	t.moveTo(x, y)
}

func (t *State) moveTo(x, y int) {
	var miny, maxy int
	if t.cur.State&cursorOrigin != 0 {
		miny = t.top
		maxy = t.bottom
	} else {
		miny = 0
		maxy = t.rows - 1
	}
	x = clamp(x, 0, t.cols-1)
	y = clamp(y, miny, maxy)
	t.dirty.markScreen()
	t.cur.State &^= cursorWrapNext
	t.cur.C = x
	t.cur.R = y
}

func (t *State) swapScreen() {
	t.screen, t.altScreen = t.altScreen, t.screen
	t.history, t.altHistory = t.altHistory, t.history
	t.mode ^= ModeAltScreen
	t.dirtyAll()
}

func (t *State) dirtyAll() {
	t.dirty.markScreen()
	for y := 0; y < t.rows; y++ {
		t.markDirtyLine(y)
	}
}

func (t *State) setScroll(top, bottom int) {
	top = clamp(top, 0, t.rows-1)
	bottom = clamp(bottom, 0, t.rows-1)
	if top > bottom {
		top, bottom = bottom, top
	}
	t.top = top
	t.bottom = bottom
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func clamp(val, min, max int) int {
	if val < min {
		return min
	} else if val > max {
		return max
	}
	return val
}

func between(val, min, max int) bool {
	if val < min || val > max {
		return false
	}
	return true
}

func IsAltMode(mode ModeFlag) bool {
	return (mode & ModeAltScreen) != 0
}

func (t *State) scrollDown(orig, n int) {
	n = clamp(n, 0, t.bottom-orig+1)

	t.clear(0, t.bottom-n+1, t.cols-1, t.bottom)
	t.dirty.markScreen()
	for i := t.bottom; i >= orig+n; i-- {
		t.screen[i], t.screen[i-n] = t.screen[i-n], t.screen[i]
		t.markDirtyLine(i)
		t.markDirtyLine(i - n)
	}

	t.dirty.Scrolled = true
	t.dirty.Scroll = Scroll{
		Up:     false,
		Origin: orig,
		Count:  n,
	}
}

func (t *State) scrollUp(orig, n int) {
	n = clamp(n, 0, t.bottom-orig+1)

	if orig == 0 && !IsAltMode(t.mode) && !t.disableHistory {
		for i := 0; i < n; i++ {
			t.wrapped = t.screen[i].IsWrapped()
			if t.disableHistory {
				continue
			}
			t.history = appendWrapped(t.history, t.screen[i])
		}
	}

	t.clear(0, orig, t.cols-1, orig+n-1)
	t.dirty.markScreen()
	for i := orig; i <= t.bottom-n; i++ {
		t.screen[i], t.screen[i+n] = t.screen[i+n], t.screen[i]
		t.markDirtyLine(i)
		t.markDirtyLine(i + n)
	}

	t.dirty.Scrolled = true
	t.dirty.Scroll = Scroll{
		Up:     true,
		Origin: orig,
		Count:  n,
	}

	// TODO: selection scroll
}

func (t *State) modMode(set bool, bit ModeFlag) {
	if set {
		t.mode |= bit
	} else {
		t.mode &^= bit
	}
}

func (t *State) setMode(priv bool, set bool, args []int) {
	if priv {
		for _, a := range args {
			switch a {
			case 1: // DECCKM - cursor key
				t.modMode(set, ModeAppCursor)
			case 5: // DECSCNM - reverse video
				mode := t.mode
				t.modMode(set, ModeReverse)
				if mode != t.mode {
					// TODO: redraw
				}
			case 6: // DECOM - origin
				if set {
					t.cur.State |= cursorOrigin
				} else {
					t.cur.State &^= cursorOrigin
				}
				t.moveAbsTo(0, 0)
			case 7: // DECAWM - auto wrap
				t.modMode(set, ModeWrap)
			// IGNORED:
			case 0, // error
				2,  // DECANM - ANSI/VT52
				3,  // DECCOLM - column
				4,  // DECSCLM - scroll
				8,  // DECARM - auto repeat
				18, // DECPFF - printer feed
				19, // DECPEX - printer extent
				42, // DECNRCM - national characters
				12: // att610 - start blinking cursor
				break
			case 25: // DECTCEM - text cursor enable mode
				t.modMode(!set, ModeHide)
			case 9: // X10 mouse compatibility mode
				t.modMode(false, ModeMouseMask)
				t.modMode(set, ModeMouseX10)
			case 1000: // report button press
				t.modMode(false, ModeMouseMask)
				t.modMode(set, ModeMouseButton)
			case 1002: // report motion on button press
				t.modMode(false, ModeMouseMask)
				t.modMode(set, ModeMouseMotion)
			case 1003: // enable all mouse motions
				t.modMode(false, ModeMouseMask)
				t.modMode(set, ModeMouseMany)
			case 1004: // send focus events to tty
				t.modMode(set, ModeFocus)
			case 1006: // extended reporting mode
				t.modMode(set, ModeMouseSgr)
			case 1034:
				t.modMode(set, Mode8bit)
			case 1049, // = 1047 and 1048
				47, 1047:
				alt := t.mode&ModeAltScreen != 0
				if alt {
					t.clear(0, 0, t.cols-1, t.rows-1)
				}
				if !set || !alt {
					t.swapScreen()
				}
				if a != 1049 {
					break
				}
				fallthrough
			case 1048:
				if set {
					t.saveCursor()
				} else {
					// TODO(cfoust): 02/27/24 is this
					// accurate? it should only apply if
					// transitioning between alt and main
					// screen
					t.restoreCursor(true)
				}
			case 1001:
				// mouse highlight mode; can hang the terminal by design when
				// implemented
			case 1005:
				// utf8 mouse mode; will confuse applications not supporting
				// utf8 and luit
			case 1015:
				// urxvt mangled mouse mode; incompatiblt and can be mistaken
				// for other control codes
			default:
				t.logf("unknown private set/reset mode %d\n", a)
			}
		}
	} else {
		for _, a := range args {
			switch a {
			case 0: // Error (ignored)
			case 2: // KAM - keyboard action
				t.modMode(set, ModeKeyboardLock)
			case 4: // IRM - insertion-replacement
				t.modMode(set, ModeInsert)
				t.logln("insert mode not implemented")
			case 12: // SRM - send/receive
				t.modMode(set, ModeEcho)
			case 20: // LNM - linefeed/newline
				t.modMode(set, ModeCRLF)
			case 34:
				t.logln("right-to-left mode not implemented")
			case 96:
				t.logln("right-to-left copy mode not implemented")
			default:
				t.logf("unknown set/reset mode %d\n", a)
			}
		}
	}
}

func (t *State) setAttr(attr []int) {
	if len(attr) == 0 {
		attr = []int{0}
	}
	for i := 0; i < len(attr); i++ {
		a := attr[i]
		switch a {
		case 0:
			t.cur.Attr.Mode &^= attrReverse | attrUnderline | attrBold | attrItalic | attrBlink
			t.cur.Attr.FG = DefaultFG
			t.cur.Attr.BG = DefaultBG
		case 1:
			t.cur.Attr.Mode |= attrBold
		case 3:
			t.cur.Attr.Mode |= attrItalic
		case 4:
			t.cur.Attr.Mode |= attrUnderline
		case 5, 6: // slow, rapid blink
			t.cur.Attr.Mode |= attrBlink
		case 7:
			t.cur.Attr.Mode |= attrReverse
		case 21, 22:
			t.cur.Attr.Mode &^= attrBold
		case 23:
			t.cur.Attr.Mode &^= attrItalic
		case 24:
			t.cur.Attr.Mode &^= attrUnderline
		case 25, 26:
			t.cur.Attr.Mode &^= attrBlink
		case 27:
			t.cur.Attr.Mode &^= attrReverse
		case 38:
			if i+2 < len(attr) && attr[i+1] == 5 {
				i += 2
				if between(attr[i], 0, 255) {
					t.cur.Attr.FG = Color(attr[i])
				} else {
					t.logf("bad fgcolor %d\n", attr[i])
				}
			} else if i+4 < len(attr) && attr[i+1] == 2 {
				i += 4
				r, g, b := attr[i-2], attr[i-1], attr[i]
				if !between(r, 0, 255) || !between(g, 0, 255) || !between(b, 0, 255) {
					t.logf("bad fg rgb color (%d,%d,%d)\n", r, g, b)
				} else {
					t.cur.Attr.FG = Color(r<<16 | g<<8 | b)
				}
			} else {
				t.logf("gfx attr %d unknown\n", a)
			}
		case 39:
			t.cur.Attr.FG = DefaultFG
		case 48:
			if i+2 < len(attr) && attr[i+1] == 5 {
				i += 2
				if between(attr[i], 0, 255) {
					t.cur.Attr.BG = Color(attr[i])
				} else {
					t.logf("bad bgcolor %d\n", attr[i])
				}
			} else if i+4 < len(attr) && attr[i+1] == 2 {
				i += 4
				r, g, b := attr[i-2], attr[i-1], attr[i]
				if !between(r, 0, 255) || !between(g, 0, 255) || !between(b, 0, 255) {
					t.logf("bad bg rgb color (%d,%d,%d)\n", r, g, b)
				} else {
					t.cur.Attr.BG = Color(r<<16 | g<<8 | b)
				}
			} else {
				t.logf("gfx attr %d unknown\n", a)
			}
		case 49:
			t.cur.Attr.BG = DefaultBG
		default:
			if between(a, 30, 37) {
				t.cur.Attr.FG = Color(a - 30)
			} else if between(a, 40, 47) {
				t.cur.Attr.BG = Color(a - 40)
			} else if between(a, 90, 97) {
				t.cur.Attr.FG = Color(a - 90 + 8)
			} else if between(a, 100, 107) {
				t.cur.Attr.BG = Color(a - 100 + 8)
			} else {
				t.logf("gfx attr %d unknown\n", a)
			}
		}
	}
}

func (t *State) insertBlanks(n int) {
	src := t.cur.C
	dst := src + n
	size := t.cols - dst
	t.dirty.markScreen()
	t.markDirtyLine(t.cur.R)

	if dst >= t.cols {
		t.clear(t.cur.C, t.cur.R, t.cols-1, t.cur.R)
	} else {
		copy(t.screen[t.cur.R][dst:dst+size], t.screen[t.cur.R][src:src+size])
		t.clear(src, t.cur.R, dst-1, t.cur.R)
	}
}

func (t *State) insertBlankLines(n int) {
	if t.cur.R < t.top || t.cur.R > t.bottom {
		return
	}
	t.scrollDown(t.cur.R, n)
}

func (t *State) deleteLines(n int) {
	if t.cur.R < t.top || t.cur.R > t.bottom {
		return
	}
	t.scrollUp(t.cur.R, n)
}

func (t *State) deleteChars(n int) {
	src := t.cur.C + n
	dst := t.cur.C
	size := t.cols - src
	t.dirty.markScreen()
	t.markDirtyLine(t.cur.R)

	if src >= t.cols {
		t.clear(t.cur.C, t.cur.R, t.cols-1, t.cur.R)
	} else {
		copy(t.screen[t.cur.R][dst:dst+size], t.screen[t.cur.R][src:src+size])
		t.clear(t.cols-n, t.cur.R, t.cols-1, t.cur.R)
	}
}

func (t *State) setTitle(title string) {
	t.dirty.Flag |= ChangedTitle
	t.title = title
}

func (t *State) Root() geom.Vec2 {
	t.RLock()
	defer t.RUnlock()

	_, history, _ := t.getFlowTarget()

	numHistory := len(history)
	root := geom.Vec2{
		R: numHistory,
	}

	if !isWrappedLines(history) {
		return root
	}

	return geom.Vec2{
		R: numHistory - 1,
		C: len(history[numHistory-1]),
	}
}

func (t *State) Size() geom.Vec2 {
	t.RLock()
	defer t.RUnlock()
	return geom.Size{
		C: t.cols,
		R: t.rows,
	}
}

func copyLine(line Line) Line {
	copied := make(Line, len(line))
	copy(copied, line)
	return copied
}

func (t *State) Screen() []Line {
	t.Lock()
	defer t.Unlock()
	return t.screen
}

func (t *State) History() []Line {
	t.Lock()
	defer t.Unlock()
	return t.history
}

func (t *State) IsAltMode() bool {
	return IsAltMode(t.mode)
}

func (t *State) String() string {
	t.Lock()
	defer t.Unlock()

	var view []rune
	for y := 0; y < t.rows; y++ {
		for x := 0; x < t.cols; x++ {
			attr := t.Cell(x, y)
			view = append(view, attr.Char)
		}
		view = append(view, '\n')
	}

	return string(view)
}
