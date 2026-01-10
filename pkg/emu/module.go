package emu

import (
	"fmt"
	"io"

	"github.com/cfoust/cy/pkg/geom"

	"github.com/mattn/go-runewidth"
)

// TODO(cfoust): 05/19/23 combine this with the other declaration
const (
	AttrReverse = 1 << iota
	AttrUnderline
	AttrBold
	AttrGfx
	AttrItalic
	AttrStrikethrough
	AttrBlink
	AttrWrap
	AttrBlank
	AttrTransparent
)

const (
	cursorDefault = 1 << iota
	cursorWrapNext
	cursorOrigin
)

// ModeFlag represents various terminal mode states.
type ModeFlag uint32

// Terminal modes
const (
	ModeWrap ModeFlag = 1 << iota
	ModeInsert
	ModeAppKeypad
	ModeAltScreen
	ModeCRLF
	ModeMouseButton
	ModeMouseMotion
	ModeReverse
	ModeKeyboardLock
	ModeHide
	ModeEcho
	ModeAppCursor
	ModeMouseSgr
	Mode8bit
	ModeBlink
	ModeFBlink
	ModeFocus
	ModeMouseX10
	ModeMouseMany
	ModeMouseMask = ModeMouseButton | ModeMouseMotion | ModeMouseX10 | ModeMouseMany
)

// ChangeFlag represents possible state changes of the terminal.
type ChangeFlag uint32

// WriteID represents the unique ID of a single contiguous Write() to the
// terminal.
type WriteID uint32

// Terminal changes to occur in VT.ReadState
const (
	ChangedScreen ChangeFlag = 1 << iota
	ChangedTitle
)

type Glyph struct {
	Char   rune
	Mode   int16
	FG, BG Color
	Write  WriteID
}

func (g Glyph) IsEmpty() bool {
	return g.Char == ' '
}

func (g Glyph) IsDefault() bool {
	return g.Mode&attrBlank != 0
}

func (g Glyph) Transparent() bool {
	return g.Mode&attrTransparent != 0
}

func (g Glyph) Width() int {
	// runewidth can be 0, but we strictly want visible glyphs to be at
	// least one cell wide.
	return geom.Max(runewidth.RuneWidth(g.Char), 1)
}

func (g Glyph) Equal(other Glyph) bool {
	return g.Char == other.Char && g.Mode == other.Mode && g.FG == other.FG &&
		g.BG == other.BG
}

// SameAttrs reports whether the two glyphs have the same visual attributes.
func (g Glyph) SameAttrs(other Glyph) bool {
	return g.Mode == other.Mode && g.FG == other.FG && g.BG == other.BG
}

func EmptyGlyph() Glyph {
	return Glyph{
		Char: ' ',
		FG:   DefaultFG,
		BG:   DefaultBG,
	}
}

type Line []Glyph

func (l Line) String() (str string) {
	for i := 0; i < len(l); i++ {
		str += string(l[i].Char)
		i += l[i].Width() - 1
	}

	return str
}

func (l Line) IsWrapped() bool {
	if len(l) == 0 {
		return false
	}

	return (l[len(l)-1].Mode & attrWrap) != 0
}

// Length returns the physical length of the line, accounting for the width of
// the final rune. Be aware that when the final rune is a double-width
// character (such as a CJK ideogram) Length can return a number that is
// greater than the number of Glyphs in the Line.
func (l Line) Length() int {
	return getLineLength(l)
}

func (l Line) Clone() Line {
	return copyLine(l)
}

// Get the occupancy state of the given line.
func (l Line) Occupancy() []bool {
	occupancy := make([]bool, len(l))
	for i := 0; i < len(l); i++ {
		if l[i].IsEmpty() {
			continue
		}

		// handle wide runes
		w := l[i].Width()
		for j := 0; j < w; j++ {
			occupancy[i+j] = true
		}
		i += geom.Max(w-1, 0)
	}

	return occupancy
}

func (l Line) IsEmpty() bool {
	occupancy := l.Occupancy()

	for _, occupied := range occupancy {
		if occupied {
			return false
		}
	}

	return true
}

// Whitespace returns the indices of the first and last non-empty cells for
// the given line.
func (l Line) Whitespace() (first, last int) {
	for i := 0; i < len(l); i++ {
		if l[i].IsEmpty() {
			continue
		}

		first = i
		break
	}

	for i := len(l) - 1; i >= 0; i-- {
		if l[i].IsEmpty() {
			continue
		}

		last = i
		break
	}

	return
}

func LineFromString(text string) Line {
	line := make(Line, 0)

	for _, r := range text {
		glyph := EmptyGlyph()
		glyph.Char = r
		line = append(line, glyph)

		// Handle wider characters
		w := runewidth.RuneWidth(r)
		if w > 1 {
			for i := 0; i < w-1; i++ {
				line = append(line, EmptyGlyph())
			}
		}
	}

	return line
}

type CursorStyle int

const (
	CursorStyleBlock CursorStyle = iota
	CursorStyleBlinkBlock
	CursorStyleSteadyBlock
	CursorStyleBlinkUnderline
	CursorStyleUnderline
	CursorStyleBlinkBar
	CursorStyleBar
)

type Cursor struct {
	geom.Vec2
	Attr  Glyph
	State uint8
	Style CursorStyle
}

type Cell struct {
	geom.Vec2
	Glyph
}

// Terminal represents the virtual terminal emulator.
type Terminal interface {
	// View displays the virtual terminal.
	View

	// Parse parses input and writes terminal changes to state.
	Parse(p []byte) (n int)

	// Write does the same as Parse, but locks first.
	io.Writer
}

// View represents the view of the virtual terminal emulator.
type View interface {
	// String dumps the virtual terminal contents.
	fmt.Stringer

	// Size returns the size of the virtual terminal.
	Size() geom.Vec2

	// Resize changes the size of the virtual terminal.
	//Resize(cols, rows int)
	Resize(geom.Vec2)

	// Mode returns the current terminal mode.
	Mode() ModeFlag

	// Title represents the title of the console window.
	Title() string

	// Directory represents the working directory of the console as specified by
	// OSC-7. For more information see:
	// https://gitlab.freedesktop.org/terminal-wg/specifications/-/issues/20
	Directory() string

	// Cell returns the glyph containing the character code, foreground color, and
	// background color at position (x, y) relative to the top left of the terminal.
	Cell(x, y int) Glyph

	// Cursor returns the current position of the cursor.
	Cursor() Cursor

	// CursorVisible returns the visible state of the cursor.
	CursorVisible() bool

	// Screen gets all of the lines on the screen.
	Screen() []Line

	// History returns the scrollback buffer.
	History() []Line

	IsAltMode() bool

	// The kind of key inputs expected by the terminal.
	KeyState() KeyProtocol

	// The location in history of the top-left cell of the screen. The `R`
	// field refers to the line in history and the `C` refers to a column
	// in that line that the cell contains. For example, when only one line
	// is in the scrollback buffer and it does not wrap onto the screen,
	// Root() will return [1, 0].
	Root() geom.Vec2

	// Flow is an API for quickly rewrapping the physical (read: unwrapped)
	// lines to fit inside of the given viewport.
	//
	// For example, with a history that looks like this:
	// ```
	// abcdefg
	// !abc
	// foobarbaz
	// ```
	// where "!" represents a viewport with `R==2` and `C==3` would
	// return:
	// ```
	// abc
	// foo
	// ```
	//
	// A viewport with `R==-2` and `C==3` would return:
	// ```
	// bcd
	// efg
	// ```
	Flow(viewport, root geom.Vec2) FlowResult

	// GetLines gets unwrapped lines from the terminal's history.
	GetLines(start, end int) []Line

	Changes() *Dirty
}

type TerminalOption func(*TerminalInfo)

type TerminalInfo struct {
	w              io.Writer
	cols, rows     int
	disableHistory bool
	historyLimit   int
}

func WithWriter(w io.Writer) TerminalOption {
	return func(info *TerminalInfo) {
		info.w = w
	}
}

func WithSize(size geom.Vec2) TerminalOption {
	return func(info *TerminalInfo) {
		info.cols = size.C
		info.rows = size.R
	}
}

// Providing WithoutHistory disables the scrollback buffer, which drastically
// reduces the amount of memory a Terminal uses.
var WithoutHistory TerminalOption = func(info *TerminalInfo) {
	info.disableHistory = true
}

// WithHistoryLimit limits the number of physical lines kept in the scrollback
// buffer. Older lines are discarded, but the coordinate system remains global.
// A non-positive limit disables this behavior.
func WithHistoryLimit(limit int) TerminalOption {
	return func(info *TerminalInfo) {
		info.historyLimit = limit
	}
}

// New returns a new virtual terminal emulator.
func New(opts ...TerminalOption) Terminal {
	info := TerminalInfo{
		w:    io.Discard,
		cols: geom.DEFAULT_SIZE.C,
		rows: geom.DEFAULT_SIZE.R,
	}
	for _, opt := range opts {
		opt(&info)
	}
	return newTerminal(info)
}
