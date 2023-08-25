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

package latte

// MouseMsg contains information about a mouse event and are sent to a programs
// update function when mouse activity occurs. Note that the mouse must first
// be enabled in order for the mouse events to be received.
type MouseMsg MouseEvent

// String returns a string representation of a mouse event.
func (m MouseMsg) String() string {
	return MouseEvent(m).String()
}

// MouseEvent represents a mouse event, which could be a click, a scroll wheel
// movement, a cursor movement, or a combination.
type MouseEvent struct {
	X    int
	Y    int
	Type MouseEventType
	Alt  bool
	Ctrl bool
}

func (m MouseEvent) Bytes() []byte {
	var flags byte = 0

	switch m.Type {
	case MouseWheelUp:
		flags |= bitWheel
		flags |= bitsWheelUp
	case MouseWheelDown:
		flags |= bitWheel
		flags |= bitsWheelDown
	case MouseLeft:
		flags |= bitsLeft
	case MouseMiddle:
		flags |= bitsMiddle
	case MouseRight:
		flags |= bitsRight
	case MouseMotion, MouseRelease:
		flags |= bitsRelease
		if m.Type == MouseMotion {
			flags |= bitMotion
		}
	}

	if m.Alt {
		flags |= bitAlt
	}

	if m.Ctrl {
		flags |= bitCtrl
	}

	return []byte{
		'\x1b',
		'[',
		'M',
		flags,
		byte(m.X) + byteOffset + 1,
		byte(m.Y) + byteOffset + 1,
	}
}

func (m MouseEvent) X10Bytes() []byte {
	b := m.Bytes()
	b[3] &= bitsLeft | bitsMiddle | bitsRight
	return b
}

// String returns a string representation of a mouse event.
func (m MouseEvent) String() (s string) {
	if m.Ctrl {
		s += "ctrl+"
	}
	if m.Alt {
		s += "alt+"
	}
	s += mouseEventTypes[m.Type]
	return s
}

// MouseEventType indicates the type of mouse event occurring.
type MouseEventType int

// Mouse event types.
const (
	MouseUnknown MouseEventType = iota
	MouseLeft
	MouseRight
	MouseMiddle
	MouseRelease
	MouseWheelUp
	MouseWheelDown
	MouseMotion
)

var mouseEventTypes = map[MouseEventType]string{
	MouseUnknown:   "unknown",
	MouseLeft:      "left",
	MouseRight:     "right",
	MouseMiddle:    "middle",
	MouseRelease:   "release",
	MouseWheelUp:   "wheel up",
	MouseWheelDown: "wheel down",
	MouseMotion:    "motion",
}

const (
	byteOffset = 32

	bitShift  = 0b0000_0100
	bitAlt    = 0b0000_1000
	bitCtrl   = 0b0001_0000
	bitMotion = 0b0010_0000
	bitWheel  = 0b0100_0000

	bitsMask = 0b0000_0011

	bitsLeft    = 0b0000_0000
	bitsMiddle  = 0b0000_0001
	bitsRight   = 0b0000_0010
	bitsRelease = 0b0000_0011

	bitsWheelUp   = 0b0000_0000
	bitsWheelDown = 0b0000_0001
)

// Parse X10-encoded mouse events; the simplest kind. The last release of X10
// was December 1986, by the way.
//
// X10 mouse events look like:
//
//	ESC [M Cb Cx Cy
//
// See: http://www.xfree86.org/current/ctlseqs.html#Mouse%20Tracking
func parseX10MouseEvent(buf []byte) MouseEvent {
	v := buf[3:6]
	var m MouseEvent
	e := v[0] - byteOffset

	if e&bitWheel != 0 {
		// Check the low two bits.
		switch e & bitsMask {
		case bitsWheelUp:
			m.Type = MouseWheelUp
		case bitsWheelDown:
			m.Type = MouseWheelDown
		}
	} else {
		// Check the low two bits.
		// We do not separate clicking and dragging.
		switch e & bitsMask {
		case bitsLeft:
			m.Type = MouseLeft
		case bitsMiddle:
			m.Type = MouseMiddle
		case bitsRight:
			m.Type = MouseRight
		case bitsRelease:
			if e&bitMotion != 0 {
				m.Type = MouseMotion
			} else {
				m.Type = MouseRelease
			}
		}
	}

	if e&bitAlt != 0 {
		m.Alt = true
	}
	if e&bitCtrl != 0 {
		m.Ctrl = true
	}

	// (1,1) is the upper left. We subtract 1 to normalize it to (0,0).
	m.X = int(v[1]) - byteOffset - 1
	m.Y = int(v[2]) - byteOffset - 1

	return m
}

// TranslateMouseEvents translates all mouse events in-place by [dx, dy].
func TranslateMouseEvents(data []byte, dx, dy int) {
	for i := 0; i < len(data); i++ {
		slice := data[i:]
		if !isMouseEvent(slice) {
			continue
		}

		e := parseX10MouseEvent(slice)
		e.X += dx
		e.Y += dy
		b := e.Bytes()
		data[i+4] = b[4]
		data[i+5] = b[5]
	}
}
