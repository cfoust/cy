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
	"github.com/cfoust/cy/pkg/geom"
)

// MouseEvent represents a mouse event, which could be a click, a scroll wheel
// movement, a cursor movement, or a combination.
type MouseEvent struct {
	geom.Vec2
	Type   MouseEventType
	Button MouseButton
	Down   bool
	Alt    bool
	Ctrl   bool
}

func (m MouseEvent) Bytes() []byte {
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

	return []byte{
		'\x1b',
		'[',
		'M',
		flags + byteOffset,
		byte(m.C) + byteOffset + 1,
		byte(m.R) + byteOffset + 1,
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
func parseX10MouseEvent(buf []byte) MouseEvent {
	v := buf[3:6]
	var m MouseEvent
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
