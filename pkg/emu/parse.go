package emu

import (
	"encoding/base64"
	"fmt"
	"strconv"

	"github.com/mattn/go-runewidth"
)

func (t *State) Print(c rune) {
	if t.mode&ModeWrap != 0 && t.cur.State&cursorWrapNext != 0 {
		t.screen[t.cur.R][t.cur.C].Mode |= attrWrap
		t.newline(true)
	}

	w := runewidth.RuneWidth(c)
	destCol := t.cur.C + w

	// TODO(cfoust): 04/03/24 this is a nasty problem, what is the expected
	// behavior? For now we just avoid an infinite loop
	if w > t.cols {
		return
	}

	// Specifically can only happen if a double-width character is printed
	// to the final cell in a row
	if destCol > t.cols {
		t.screen[t.cur.R][t.cur.C].Mode |= attrWrap
		t.newline(true)
		t.Print(c)
		return
	}

	t.setChar(c, &t.cur.Attr, t.cur.C, t.cur.R)
	if destCol < t.cols {
		t.moveTo(destCol, t.cur.R)
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
		t.moveTo(t.cur.C-1, t.cur.R)
	// CR
	case '\r':
		t.moveTo(0, t.cur.R)
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
	t.dirty.hookState[t.dirty.hookCount] = b
	t.dirty.hookCount++
	// TODO(cfoust): 08/10/23
	//fmt.Printf("[Put] %02x\n", b)
}

func (t *State) Unhook() {
	hook := string(t.dirty.hookState[0:t.dirty.hookCount])

	_, ok := t.dirty.hooks[hook]
	if !ok {
		return
	}

	t.dirty.hooks[hook] = true

	// TODO(cfoust): 08/10/23
	//fmt.Printf("[Unhook]\n")
}

func (t *State) Hook(
	params []int64,
	intermediates []byte,
	ignore bool,
	r rune,
) {
	t.dirty.hookCount = 1
	t.dirty.hookState[0] = byte(r)
	// TODO(cfoust): 08/10/23
	//fmt.Printf("[Hook] params=%v, intermediates=%v, ignore=%v, r=%v\n", params, intermediates, ignore, r)
}

func (t *State) OscDispatch(params [][]byte, bellTerminated bool) {
	if len(params) == 0 {
		return
	}

	args := make([]string, 0)
	for _, arg := range params {
		args = append(args, string(arg))
	}

	s := strEscape{
		typ:  rune(params[0][0]),
		args: args,
	}

	var p *string
	switch d := s.arg(0, 0); d {
	case 0, 1, 2:
		title := s.argString(1, "")
		if title != "" {
			t.setTitle(title)
		}
	case 7:
		t.directory = s.argString(1, "")
	case 10:
		if len(s.args) < 2 {
			break
		}

		c := s.argString(1, "")
		p := &c
		if p != nil && *p == "?" {
			t.oscColorResponse(DefaultFG, 10)
		} else if err := t.setColorName(DefaultFG, p); err != nil {
			t.logf("invalid foreground color: %s\n", maybe(p))
		}
		// TODO: redraw when color is set
	case 11:
		if len(s.args) < 2 {
			break
		}

		c := s.argString(1, "")
		p := &c
		if p != nil && *p == "?" {
			t.oscColorResponse(DefaultBG, 11)
		} else if err := t.setColorName(DefaultBG, p); err != nil {
			t.logf("invalid cursor color: %s\n", maybe(p))
		}
		// TODO: redraw when color is set
	// case 12:
	// if len(s.args) < 2 {
	// 	break
	// }

	// c := s.argString(1, "")
	// p := &c
	// if p != nil && *p == "?" {
	// 	t.oscColorResponse(int(DefaultCursor), 12)
	// } else if err := t.setColorName(int(DefaultCursor), p); err != nil {
	// 	t.logf("invalid background color: %s\n", p)
	// } else {
	// 	// TODO: redraw
	// }
	case 4: // color set
		if len(s.args) < 3 {
			break
		}

		c := s.argString(2, "")
		p = &c
		fallthrough
	case 104: // color reset
		j := -1
		if len(s.args) > 1 {
			j = s.arg(1, 0)
		}
		if p != nil && *p == "?" { // report
			t.osc4ColorResponse(XTermColor(j))
		} else if err := t.setColorName(XTermColor(j), p); err != nil {
			if d != 104 || len(s.args) > 1 {
				t.logf("invalid color j=%d, p=%s\n", j, maybe(p))
			}
		}
	case 52: // clipboard operations
		t.handleOSC52(s)
	case 133: // semantic prompt (FinalTerm/iTerm2 shell integration)
		t.handleOSC133(s)
	default:
		t.logf("unknown OSC command %d\n", d)
		// TODO: s.dump()
	}

	// TODO(cfoust): 09/15/24 handle this?
	//case 'k': // old title set compatibility
	//title := s.argString(0, "")
	//if title != "" {
	//t.setTitle(title)
	//}
	//default:
	//// TODO: Ignore these codes instead of complain?
	//// 'P': // DSC - device control string
	//// '_': // APC - application program command
	//// '^': // PM - privacy message

	//t.logf("unhandled STR sequence '%c'\n", s.typ)
	//// t.str.dump()
}

func (t *State) CsiDispatch(
	params []int64,
	intermediates []byte,
	ignore bool,
	r rune,
) {
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
		t.moveTo(t.cur.C, t.cur.R-c.maxarg(0, 1))
	case 'B', 'e': // CUD, VPR - cursor <n> down
		t.moveTo(t.cur.C, t.cur.R+c.maxarg(0, 1))
	case 'c': // DA - device attributes
		if c.arg(0, 0) == 0 {
			_, _ = t.w.Write([]byte("\033[?6c"))
		}
	case 'C', 'a': // CUF, HPR - cursor <n> forward
		t.moveTo(t.cur.C+c.maxarg(0, 1), t.cur.R)
	case 'D': // CUB - cursor <n> backward
		t.moveTo(t.cur.C-c.maxarg(0, 1), t.cur.R)
	case 'E': // CNL - cursor <n> down and first col
		t.moveTo(0, t.cur.R+c.arg(0, 1))
	case 'F': // CPL - cursor <n> up and first col
		t.moveTo(0, t.cur.R-c.arg(0, 1))
	case 'g': // TBC - tabulation clear
		switch c.arg(0, 0) {
		// clear current tab stop
		case 0:
			t.tabs[t.cur.C] = false
		// clear all tabs
		case 3:
			for i := range t.tabs {
				t.tabs[i] = false
			}
		default:
			goto unknown
		}
	case 'G', '`': // CHA, HPA - Move to <col>
		t.moveTo(c.arg(0, 1)-1, t.cur.R)
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
			t.clear(t.cur.C, t.cur.R, t.cols-1, t.cur.R)
			if t.cur.R < t.rows-1 {
				t.clear(0, t.cur.R+1, t.cols-1, t.rows-1)
			}
		case 1: // above
			if t.cur.R > 1 {
				t.clear(0, 0, t.cols-1, t.cur.R-1)
			}
			t.clear(0, t.cur.R, t.cur.C, t.cur.R)
		case 2: // all
			t.clear(0, 0, t.cols-1, t.rows-1)
		default:
			goto unknown
		}
	case 'K': // EL - clear line
		switch c.arg(0, 0) {
		case 0: // right
			t.clear(t.cur.C, t.cur.R, t.cols-1, t.cur.R)
		case 1: // left
			t.clear(0, t.cur.R, t.cur.C, t.cur.R)
		case 2: // all
			t.clear(0, t.cur.R, t.cols-1, t.cur.R)
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
		t.clear(t.cur.C, t.cur.R, t.cur.C+c.arg(0, 1)-1, t.cur.R)
	case 'P': // DCH - delete <n> chars
		t.deleteChars(c.arg(0, 1))
	case 'Z': // CBT - cursor backward tabulation <n> tab stops
		n := c.arg(0, 1)
		for i := 0; i < n; i++ {
			t.putTab(false)
		}
	case 'd': // VPA - move to <row>
		t.moveAbsTo(t.cur.C, c.arg(0, 1)-1)
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
			_, _ = t.w.Write([]byte("\033[0n"))
		case 6: // CPR - cursor position report
			_, _ = fmt.Fprintf(t.w, "\033[%d;%dR", t.cur.R+1, t.cur.C+1)
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
	case 'u': // DECRC - restore cursor position (ANSI.SYS) OR Kitty keyboard protocol
		if t.handleKittyProtocol(&c) {
			return
		}

		// Fallback to DECRC - restore cursor position
		t.restoreCursor(false)
	case 'q': // DECSCUSR - set cursor style
		style := CursorStyleBlock
		val := c.arg(0, 0)
		if 0 < val && val <= 6 {
			style = CursorStyle(val)
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
		fmt.Printf(
			"[EscDispatch] %c intermediates=%v, ignore=%v, byte=%02x\n",
			b,
			intermediates,
			ignore,
			b,
		)
	case 'D': // IND - linefeed
		if t.cur.R == t.bottom {
			t.scrollUp(t.top, 1)
		} else {
			t.moveTo(t.cur.C, t.cur.R+1)
		}
	case 'E': // NEL - next line
		t.newline(true)
	case 'H': // HTS - horizontal tab stop
		t.tabs[t.cur.C] = true
	case 'M': // RI - reverse index
		if t.cur.R == t.top {
			t.scrollDown(t.top, 1)
		} else {
			t.moveTo(t.cur.C, t.cur.R-1)
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
		t.restoreCursor(false)
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

// handleOSC52 processes OSC 52 clipboard sequences
func (t *State) handleOSC52(s strEscape) {
	if len(s.args) < 3 {
		return
	}

	// OSC 52 format: 52;Pc;Pd
	// Pc = clipboard identifier ('c' for clipboard, 'p' for primary, 's' for select)
	// Pd = base64-encoded data or '?' to query
	pc := s.argString(1, "")
	pd := s.argString(2, "")

	// We only support clipboard ('c') for now
	if pc != "c" {
		return
	}

	// Query clipboard
	if pd == "?" {
		t.logf("osc-52: query not supported yet")
		return
	}

	// Decode base64 data
	_, err := base64.StdEncoding.DecodeString(pd)
	if err != nil {
		t.logf("osc-52: failed to decode data %v", err)
		return
	}

	// TODO(cfoust): 08/10/25 publish event
}

// handleOSC133 processes OSC 133 semantic prompt sequences.
//
// Format: OSC 133 ; <marker> [; <params>] ST
//   - A: Prompt started
//   - B: Command started (user input begins)
//   - C: Command executed
//   - D[;<exit-code>]: Command finished with optional exit code
func (t *State) handleOSC133(s strEscape) {
	if len(s.args) < 2 {
		return
	}

	marker := s.argString(1, "")
	if len(marker) == 0 {
		return
	}

	writeID := t.dirty.LastWrite()

	switch marker[0] {
	case 'A': // Prompt started
		t.dirty.AddSemanticPrompt(PromptStart, writeID, nil)
	case 'B': // Command started (user input begins)
		t.dirty.AddSemanticPrompt(CommandStart, writeID, nil)
	case 'C': // Command executed
		t.dirty.AddSemanticPrompt(CommandExecuted, writeID, nil)
	case 'D': // Command finished with optional exit code
		var exitCode *int
		if len(s.args) >= 3 {
			if code, err := strconv.Atoi(s.argString(2, "")); err == nil {
				exitCode = &code
			}
		}
		t.dirty.AddSemanticPrompt(CommandFinished, writeID, exitCode)
	}
}
