package emu

import (
	"fmt"
	"io"
	"io/ioutil"

	"github.com/cfoust/cy/pkg/geom"
)

// Terminal represents the virtual terminal emulator.
type Terminal interface {
	// View displays the virtual terminal.
	View

	// Write parses input and writes terminal changes to state.
	io.Writer
}

// View represents the view of the virtual terminal emulator.
type View interface {
	// String dumps the virtual terminal contents.
	fmt.Stringer

	// Size returns the size of the virtual terminal.
	Size() (cols, rows int)

	// Resize changes the size of the virtual terminal.
	Resize(cols, rows int)

	// Mode returns the current terminal mode.//
	Mode() ModeFlag

	// Title represents the title of the console window.
	Title() string

	// Cell returns the glyph containing the character code, foreground color, and
	// background color at position (x, y) relative to the top left of the terminal.
	Cell(x, y int) Glyph

	// Cursor returns the current position of the cursor.
	Cursor() Cursor

	// CursorVisible returns the visible state of the cursor.
	CursorVisible() bool

	// Screen gets all of the lines on the screen.
	Screen() []Line

	// History returns the full scrollback buffer and current screen.
	History() []Line
}

type TerminalOption func(*TerminalInfo)

type TerminalInfo struct {
	w          io.Writer
	cols, rows int
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

// New returns a new virtual terminal emulator.
func New(opts ...TerminalOption) Terminal {
	info := TerminalInfo{
		w:    ioutil.Discard,
		cols: 80,
		rows: 24,
	}
	for _, opt := range opts {
		opt(&info)
	}
	return newTerminal(info)
}
