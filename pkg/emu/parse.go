package emu

import (
	"fmt"

	"github.com/mattn/go-runewidth"
	"github.com/rs/zerolog/log"
)

func (t *State) Print(c rune) {
	if t.mode&ModeWrap != 0 && t.cur.State&cursorWrapNext != 0 {
		log.Info().Msgf("line %d wrapped", t.cur.Y)
		t.lines[t.cur.Y][t.cur.X].Mode |= attrWrap
		t.newline(true)
	}

	w := runewidth.RuneWidth(c)
	t.setChar(c, &t.cur.Attr, t.cur.X, t.cur.Y)
	if t.cur.X+w < t.cols {
		t.moveTo(t.cur.X+w, t.cur.Y)
	} else {
		t.cur.State |= cursorWrapNext
	}
}

func (t *State) Execute(b byte) {
	switch b {
	// HT
	case '\t':
		t.putTab(true)
	// BS
	case '\b':
		t.moveTo(t.cur.X-1, t.cur.Y)
	// CR
	case '\r':
		t.moveTo(0, t.cur.Y)
	// LF, VT, LF
	case '\f', '\v', '\n':
		// go to first col if mode is set
		t.newline(t.mode&ModeCRLF != 0)
	// BEL
	case '\a':
		// TODO: emit sound
		// TODO: window alert if not focused
	}
}

func (t *State) Put(b byte) {
	// TODO(cfoust): 08/10/23
	//fmt.Printf("[Put] %02x\n", b)
}

func (t *State) Unhook() {
	// TODO(cfoust): 08/10/23
	//fmt.Printf("[Unhook]\n")
}

func (t *State) Hook(params []int64, intermediates []byte, ignore bool, r rune) {
	// TODO(cfoust): 08/10/23
	//fmt.Printf("[Hook] params=%v, intermediates=%v, ignore=%v, r=%v\n", params, intermediates, ignore, r)
}

func (t *State) OscDispatch(params [][]byte, bellTerminated bool) {
	// TODO(cfoust): 08/10/23 do we care about operating system commands?
	// I can only find one reference to them here: https://vt100.net/docs/vt510-rm/chapter4.html
	//fmt.Printf("[OscDispatch] params=%v, bellTerminated=%v\n", params, bellTerminated)
}

func (t *State) CsiDispatch(params []int64, intermediates []byte, ignore bool, r rune) {
	args := make([]int, 0)
	for _, arg := range params {
		args = append(args, int(arg))
	}

	// go-vte returns _always_ returns a params array (unnecessarily)
	if len(args) == 1 && args[0] == 0 {
		args = make([]int, 0)
	}

	c := csiEscape{
		args:          args,
		intermediates: intermediates,
		mode:          byte(r),
		priv:          len(intermediates) > 0 && intermediates[0] == '?',
	}

	// when in doubt, see https://invisible-island.net/xterm/ctlseqs/ctlseqs.html
	switch c.mode {
	default:
		goto unknown
	case '@': // ICH - insert <n> blank char
		t.insertBlanks(c.arg(0, 1))
	case 'A': // CUU - cursor <n> up
		t.moveTo(t.cur.X, t.cur.Y-c.maxarg(0, 1))
	case 'B', 'e': // CUD, VPR - cursor <n> down
		t.moveTo(t.cur.X, t.cur.Y+c.maxarg(0, 1))
	case 'c': // DA - device attributes
		if c.arg(0, 0) == 0 {
			// TODO: write vt102 id
		}
	case 'C', 'a': // CUF, HPR - cursor <n> forward
		t.moveTo(t.cur.X+c.maxarg(0, 1), t.cur.Y)
	case 'D': // CUB - cursor <n> backward
		t.moveTo(t.cur.X-c.maxarg(0, 1), t.cur.Y)
	case 'E': // CNL - cursor <n> down and first col
		t.moveTo(0, t.cur.Y+c.arg(0, 1))
	case 'F': // CPL - cursor <n> up and first col
		t.moveTo(0, t.cur.Y-c.arg(0, 1))
	case 'g': // TBC - tabulation clear
		switch c.arg(0, 0) {
		// clear current tab stop
		case 0:
			t.tabs[t.cur.X] = false
		// clear all tabs
		case 3:
			for i := range t.tabs {
				t.tabs[i] = false
			}
		default:
			goto unknown
		}
	case 'G', '`': // CHA, HPA - Move to <col>
		t.moveTo(c.arg(0, 1)-1, t.cur.Y)
	case 'H', 'f': // CUP, HVP - move to <row> <col>
		t.moveAbsTo(c.arg(1, 1)-1, c.arg(0, 1)-1)
	case 'I': // CHT - cursor forward tabulation <n> tab stops
		n := c.arg(0, 1)
		for i := 0; i < n; i++ {
			t.putTab(true)
		}
	case 'J': // ED - clear screen
		// TODO: sel.ob.x = -1
		switch c.arg(0, 0) {
		case 0: // below
			t.clear(t.cur.X, t.cur.Y, t.cols-1, t.cur.Y)
			if t.cur.Y < t.rows-1 {
				t.clear(0, t.cur.Y+1, t.cols-1, t.rows-1)
			}
		case 1: // above
			if t.cur.Y > 1 {
				t.clear(0, 0, t.cols-1, t.cur.Y-1)
			}
			t.clear(0, t.cur.Y, t.cur.X, t.cur.Y)
		case 2: // all
			t.clear(0, 0, t.cols-1, t.rows-1)
		default:
			goto unknown
		}
	case 'K': // EL - clear line
		switch c.arg(0, 0) {
		case 0: // right
			t.clear(t.cur.X, t.cur.Y, t.cols-1, t.cur.Y)
		case 1: // left
			t.clear(0, t.cur.Y, t.cur.X, t.cur.Y)
		case 2: // all
			t.clear(0, t.cur.Y, t.cols-1, t.cur.Y)
		}
	case 'S': // SU - scroll <n> lines up
		t.scrollUp(t.top, c.arg(0, 1))
	case 'T': // SD - scroll <n> lines down
		t.scrollDown(t.top, c.arg(0, 1))
	case 'L': // IL - insert <n> blank lines
		t.insertBlankLines(c.arg(0, 1))
	case 'l': // RM - reset mode
		t.setMode(c.priv, false, c.args)
	case 'M': // DL - delete <n> lines
		t.deleteLines(c.arg(0, 1))
	case 'X': // ECH - erase <n> chars
		t.clear(t.cur.X, t.cur.Y, t.cur.X+c.arg(0, 1)-1, t.cur.Y)
	case 'P': // DCH - delete <n> chars
		t.deleteChars(c.arg(0, 1))
	case 'Z': // CBT - cursor backward tabulation <n> tab stops
		n := c.arg(0, 1)
		for i := 0; i < n; i++ {
			t.putTab(false)
		}
	case 'd': // VPA - move to <row>
		t.moveAbsTo(t.cur.X, c.arg(0, 1)-1)
	case 'h': // SM - set terminal mode
		t.setMode(c.priv, true, c.args)
	case 'm': // SGR - terminal attribute (color)
		switch c.intermediate(0, 0) {
		case '>': // XTMODKEYS
		case '?': // XTQMODKEYS
		default:
			t.setAttr(c.args)
		}
	case 'n':
		switch c.arg(0, 0) {
		case 5: // DSR - device status report
			t.w.Write([]byte("\033[0n"))
		case 6: // CPR - cursor position report
			t.w.Write([]byte(fmt.Sprintf("\033[%d;%dR", t.cur.Y+1, t.cur.X+1)))
		}
	case 'r': // DECSTBM - set scrolling region
		if c.priv {
			goto unknown
		} else {
			t.setScroll(c.arg(0, 1)-1, c.arg(1, t.rows)-1)
			t.moveAbsTo(0, 0)
		}
	case 's': // DECSC - save cursor position (ANSI.SYS)
		t.saveCursor()
	case 'u': // DECRC - restore cursor position (ANSI.SYS)
		t.restoreCursor()
	case 'q': // DECSCUSR - set cursor style
		style := CursorStyleBlock
		switch c.arg(0, 0) {
		case 2:
			style = CursorStyleSteadyBlock
		case 3:
			style = CursorStyleUnderline
		case 4:
			style = CursorStyleBlinkUnderline
		case 5:
			style = CursorStyleBar
		case 6:
			style = CursorStyleBlinkBar
		}
		t.cur.Style = style
	case 't': // XTWINOPS - window manipulation (ignored)
	case '0', '1', '2', '3', '4', '5', '6':
	}
	return

unknown: // TODO: get rid of this goto
	fmt.Printf("[CsiDispatch] params=%v, intermediates=%v, ignore=%v, r=%v\n", params, intermediates, ignore, r)
}

func (t *State) EscDispatch(intermediates []byte, ignore bool, b byte) {
	switch b {
	default:
		fmt.Printf("[EscDispatch] %c intermediates=%v, ignore=%v, byte=%02x\n", b, intermediates, ignore, b)
	case 'D': // IND - linefeed
		if t.cur.Y == t.bottom {
			t.scrollUp(t.top, 1)
		} else {
			t.moveTo(t.cur.X, t.cur.Y+1)
		}
	case 'E': // NEL - next line
		t.newline(true)
	case 'H': // HTS - horizontal tab stop
		t.tabs[t.cur.X] = true
	case 'M': // RI - reverse index
		if t.cur.Y == t.top {
			t.scrollDown(t.top, 1)
		} else {
			t.moveTo(t.cur.X, t.cur.Y-1)
		}
	case 'Z': // DECID - identify terminal
		// TODO: write to our writer our id
	case 'c': // RIS - reset to initial state
		t.reset()
	case '=': // DECPAM - application keypad
		t.mode |= ModeAppKeypad
	case '>': // DECPNM - normal keypad
		t.mode &^= ModeAppKeypad
	case '7': // DECSC - save cursor
		t.saveCursor()
	case '8': // DECRC - restore cursor
		t.restoreCursor()
	case '\\': // ST - stop

	// Character Sets (G0 and G1 Designators)
	case '0': // line drawing set
		t.cur.Attr.Mode |= attrGfx
	case 'B': // USASCII
		t.cur.Attr.Mode &^= attrGfx
	case 'A', // UK (ignored)
		'<', // multinational (ignored)
		'5', // Finnish (ignored)
		'C', // Finnish (ignored)
		'K': // German (ignored)
	}
}
