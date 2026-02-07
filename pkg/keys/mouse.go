/*
*
This file is a heavily modified version of key.go as it appear{s,ed} in
https://github.com/charmbracelet/bubbletea with the following LICENSE:

MIT License

# Copyright (c) 2020-2023 Charmbracelet, Inc

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

package keys

import (
	"fmt"
	"strconv"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
)

// Mouse represents a mouse event, which could be a click, a scroll wheel
// movement, a cursor movement, or a combination.
type Mouse struct {
	geom.Vec2
	Type   MouseEventType
	Button MouseButton
	Down   bool
	Alt    bool
	Ctrl   bool
}

// mouseFlags computes the flags byte common to both X10 and SGR
// encodings.
func (m Mouse) mouseFlags() byte {
	var flags byte = 0

	switch m.Button {
	case MouseLeft:
		flags |= bitsLeft
	case MouseMiddle:
		flags |= bitsMiddle
	case MouseRight:
		flags |= bitsRight
	}

	if !m.Down {
		flags |= bitsRelease
	}

	switch m.Type {
	case MousePress:
		switch m.Button {
		case MouseWheelUp:
			flags |= bitWheel
			flags |= bitsWheelUp
		case MouseWheelDown:
			flags |= bitWheel
			flags |= bitsWheelDown
		case MouseWheelRight:
			flags |= bitWheel
			flags |= bitsWheelRight
		case MouseWheelLeft:
			flags |= bitWheel
			flags |= bitsWheelLeft
		}
	case MouseMotion:
		flags |= bitMotion
	}

	if m.Alt {
		flags |= bitAlt
	}

	if m.Ctrl {
		flags |= bitCtrl
	}

	return flags
}

func (m Mouse) x10Bytes() []byte {
	flags := m.mouseFlags()
	return []byte{
		'\x1b',
		'[',
		'M',
		flags + byteOffset,
		byte(m.C) + byteOffset + 1,
		byte(m.R) + byteOffset + 1,
	}
}

// sgrBytes serializes the mouse event in SGR (mode 1006) format:
//
//	ESC [ < button ; col ; row { M | m }
//
// Coordinates are 1-indexed. The trailing character is 'M' for
// press/motion and 'm' for release.
func (m Mouse) sgrBytes() []byte {
	flags := m.mouseFlags()

	// SGR doesn't use bitsRelease; instead the suffix encodes
	// press vs release.
	if !m.Down {
		flags &^= bitsRelease
	}

	suffix := byte('M')
	if m.Type == MousePress && !m.Down {
		suffix = 'm'
	}

	return []byte(fmt.Sprintf(
		"\x1b[<%d;%d;%d%c",
		flags,
		m.C+1,
		m.R+1,
		suffix,
	))
}

// Bytes serializes the mouse event for the given terminal mode,
// returning the encoded bytes and whether the event should be
// reported at all. The tracking mode (ModeMouseX10, ModeMouseButton,
// etc.) determines which event types are forwarded; ModeMouseSgr
// selects the wire encoding.
func (m Mouse) Bytes(mode emu.ModeFlag) ([]byte, bool) {
	switch mode & emu.ModeMouseMask {
	case emu.ModeMouseX10:
		if m.Type != MousePress {
			return nil, false
		}
	case emu.ModeMouseButton:
		if m.Type == MouseMotion {
			return nil, false
		}
	case emu.ModeMouseMotion, emu.ModeMouseMany:
		// Report everything.
	default:
		return nil, false
	}

	if mode&emu.ModeMouseSgr != 0 {
		return m.sgrBytes(), true
	}

	b := m.x10Bytes()
	if mode&emu.ModeMouseMask == emu.ModeMouseX10 {
		b[3] &= bitsLeft | bitsMiddle | bitsRight
	}
	return b, true
}

// String returns a string representation of a mouse event.
func (m Mouse) String() (s string) {
	if m.Ctrl {
		s += "ctrl+"
	}
	if m.Alt {
		s += "alt+"
	}
	s += mouseButtonTypes[m.Button]

	switch m.Type {
	case MousePress:
		if m.Down {
			s += " (down)"
		} else {
			s += " (up)"
		}
	case MouseMotion:
		if m.Down {
			s += " (dragging)"
		} else {
			s += " (motion)"
		}
	}

	return s
}

// MouseEventType indicates the type of mouse event occurring.
type MouseEventType int

// Mouse event types.
const (
	MouseUnknown MouseEventType = iota
	MousePress
	MouseMotion
)

type MouseButton int

const (
	MouseLeft MouseButton = iota
	MouseRight
	MouseMiddle
	MouseWheelUp
	MouseWheelDown
	MouseWheelRight
	MouseWheelLeft
)

var mouseButtonTypes = map[MouseButton]string{
	MouseLeft:       "left",
	MouseRight:      "right",
	MouseMiddle:     "middle",
	MouseWheelUp:    "wheel up",
	MouseWheelDown:  "wheel down",
	MouseWheelLeft:  "wheel left",
	MouseWheelRight: "wheel right",
}

const (
	byteOffset = 32

	bitShift = 0b0000_0100
	bitAlt   = 0b0000_1000
	bitCtrl  = 0b0001_0000

	bitType   = 0b0110_0000
	bitPress  = 0b0000_0000
	bitMotion = 0b0010_0000
	bitWheel  = 0b0100_0000

	bitsMask = 0b0000_0011

	bitsLeft    = 0b0000_0000
	bitsMiddle  = 0b0000_0001
	bitsRight   = 0b0000_0010
	bitsRelease = 0b0000_0011

	bitsWheelUp    = 0b0000_0000
	bitsWheelDown  = 0b0000_0001
	bitsWheelLeft  = 0b0000_0010
	bitsWheelRight = 0b0000_0011
)

// Parse X10-encoded mouse events; the simplest kind. The last release of X10
// was December 1986, by the way.
//
// X10 mouse events look like:
//
//	ESC [M Cb Cx Cy
//
// See: http://www.xfree86.org/current/ctlseqs.html#Mouse%20Tracking
func parseX10MouseEvent(buf []byte) Mouse {
	v := buf[3:6]
	var m Mouse
	e := v[0] - byteOffset

	m.Down = true
	switch e & bitsMask {
	case bitsLeft:
		m.Button = MouseLeft
	case bitsMiddle:
		m.Button = MouseMiddle
	case bitsRight:
		m.Button = MouseRight
	case bitsRelease:
		m.Down = false
	}

	switch e & bitType {
	case bitPress:
		m.Type = MousePress
	case bitWheel:
		m.Type = MousePress
		// Check the low two bits.
		switch e & bitsMask {
		case bitsWheelUp:
			m.Button = MouseWheelUp
		case bitsWheelDown:
			m.Button = MouseWheelDown
		case bitsWheelLeft:
			m.Button = MouseWheelLeft
		case bitsWheelRight:
			m.Button = MouseWheelRight
		}
	case bitMotion:
		m.Type = MouseMotion
	}

	if e&bitAlt != 0 {
		m.Alt = true
	}
	if e&bitCtrl != 0 {
		m.Ctrl = true
	}

	// (1,1) is the upper left. We subtract 1 to normalize it to (0,0).
	m.C = int(v[1]) - byteOffset - 1
	m.R = int(v[2]) - byteOffset - 1

	return m
}

func isMouseEvent(b []byte) bool {
	return len(b) >= 6 && b[0] == '\x1b' && b[1] == '[' && b[2] == 'M'
}

// isSGRMouseEvent checks whether b begins with an SGR mouse sequence
// (ESC [ <).
func isSGRMouseEvent(b []byte) bool {
	return len(b) >= 3 && b[0] == '\x1b' && b[1] == '[' && b[2] == '<'
}

// parseSGRMouseEvent parses an SGR-encoded mouse event:
//
//	ESC [ < button ; col ; row { M | m }
//
// Coordinates are 1-indexed. Returns the Mouse event and the number
// of bytes consumed. If the sequence is incomplete or malformed, ok
// is false.
func parseSGRMouseEvent(
	buf []byte,
) (m Mouse, consumed int, ok bool) {
	// Minimum: ESC [ < digit ; digit ; digit M  →  9 bytes
	if len(buf) < 9 {
		return Mouse{}, 0, false
	}

	// Find the terminating 'M' or 'm'.
	end := -1
	for i := 3; i < len(buf); i++ {
		if buf[i] == 'M' || buf[i] == 'm' {
			end = i
			break
		}
		// Only digits and semicolons are valid in the parameter area.
		if buf[i] != ';' && (buf[i] < '0' || buf[i] > '9') {
			return Mouse{}, 0, false
		}
	}
	if end == -1 {
		return Mouse{}, 0, false
	}

	// Parse the three semicolon-separated decimal values.
	params := string(buf[3:end])
	var parts [3]string
	pi := 0
	start := 0
	for i := 0; i <= len(params); i++ {
		if i == len(params) || params[i] == ';' {
			if pi >= 3 {
				return Mouse{}, 0, false
			}
			parts[pi] = params[start:i]
			pi++
			start = i + 1
		}
	}
	if pi != 3 {
		return Mouse{}, 0, false
	}

	button, err := strconv.Atoi(parts[0])
	if err != nil {
		return Mouse{}, 0, false
	}
	col, err := strconv.Atoi(parts[1])
	if err != nil {
		return Mouse{}, 0, false
	}
	row, err := strconv.Atoi(parts[2])
	if err != nil {
		return Mouse{}, 0, false
	}

	e := byte(button)

	m.Down = true
	if buf[end] == 'm' {
		// SGR release: 'm' suffix preserves button identity.
		m.Down = false
	}

	switch e & bitType {
	case bitPress:
		m.Type = MousePress
		// For press events with 'm' suffix, it's a release.
		if buf[end] == 'm' {
			m.Type = MousePress
		}
	case bitWheel:
		m.Type = MousePress
		switch e & bitsMask {
		case bitsWheelUp:
			m.Button = MouseWheelUp
		case bitsWheelDown:
			m.Button = MouseWheelDown
		case bitsWheelLeft:
			m.Button = MouseWheelLeft
		case bitsWheelRight:
			m.Button = MouseWheelRight
		}
		// SGR coordinates are 1-indexed.
		m.C = col - 1
		m.R = row - 1
		if e&bitAlt != 0 {
			m.Alt = true
		}
		if e&bitCtrl != 0 {
			m.Ctrl = true
		}
		return m, end + 1, true
	case bitMotion:
		m.Type = MouseMotion
	}

	// Decode button for non-wheel events.
	if e&bitType != bitWheel {
		switch e & bitsMask {
		case bitsLeft:
			m.Button = MouseLeft
		case bitsMiddle:
			m.Button = MouseMiddle
		case bitsRight:
			m.Button = MouseRight
		case bitsRelease:
			// In SGR mode the 'm' suffix handles release,
			// but bitsRelease can still appear for
			// compatibility.
			m.Down = false
		}
	}

	if e&bitAlt != 0 {
		m.Alt = true
	}
	if e&bitCtrl != 0 {
		m.Ctrl = true
	}

	// SGR coordinates are 1-indexed.
	m.C = col - 1
	m.R = row - 1

	return m, end + 1, true
}
