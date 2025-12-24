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
	"strings"
)

// KeyEventType represents the type of key event (press/repeat/release)
type KeyEventType int

const (
	KeyEventPress KeyEventType = iota
	KeyEventRepeat
	KeyEventRelease
)

func (k KeyEventType) String() string {
	switch k {
	case KeyEventPress:
		return "press"
	case KeyEventRepeat:
		return "repeat"
	case KeyEventRelease:
		return "release"
	default:
		return "unknown"
	}
}

// KeyModifiers represents Kitty protocol modifier flags
type KeyModifiers int

const (
	KeyModShift KeyModifiers = 1 << iota
	KeyModAlt
	KeyModCtrl
	KeyModSuper
	KeyModHyper
	KeyModMeta
	KeyModCapsLock
	KeyModNumLock
)

const (
	// Special key code that indicates this Key only consists of content in
	// the Text field and nothing else. This is supported in bubbletea, so
	// we need to handle it somehow for backwards compatibility.
	KeyText = KittyKeyF12 + 1
)

// Key contains information about a keypress using Kitty protocol structure.
//
// The fields in this structure have meanings as defined by the Kitty keyboard
// protocol: https://sw.kovidgoyal.net/kitty/keyboard-protocol/#an-overview
type Key struct {
	Code    rune
	Shifted rune
	Base    rune
	Type    KeyEventType // Press/repeat/release
	Mod     KeyModifiers // Combined modifier flags
	Text    string
}

// String returns a friendly string representation for a key. It's safe (and
// encouraged) for use in key comparison.
//
//	k := Key{KeyCode: KittyKeyEnter}
//	fmt.Println(k)
//	// Output: enter
func (k Key) String() (str string) {
	var modParts []string

	if k.HasSuper() {
		modParts = append(modParts, "super")
	}
	if k.HasHyper() {
		modParts = append(modParts, "hyper")
	}
	if k.HasMeta() {
		modParts = append(modParts, "meta")
	}
	if k.HasCtrl() {
		modParts = append(modParts, "ctrl")
	}
	if k.HasAlt() {
		modParts = append(modParts, "alt")
	}
	if k.HasShift() {
		modParts = append(modParts, "shift")
	}

	var keyPart string
	if k.Code == 0 {
		keyPart = "unknown"
	} else {
		keyCode := k.Code
		switch keyCode {
		case KittyKeyEscape:
			keyPart = "escape"
		case KittyKeyEnter:
			keyPart = "enter"
		case KittyKeyTab:
			keyPart = "tab"
		case KittyKeyBackspace:
			keyPart = "backspace"
		case KittyKeyInsert:
			keyPart = "insert"
		case KittyKeyDelete:
			keyPart = "delete"
		case KittyKeyHome:
			keyPart = "home"
		case KittyKeyEnd:
			keyPart = "end"
		case KittyKeyPageUp:
			keyPart = "pgup"
		case KittyKeyPageDown:
			keyPart = "pgdown"
		case KittyKeyLeft:
			keyPart = "left"
		case KittyKeyUp:
			keyPart = "up"
		case KittyKeyRight:
			keyPart = "right"
		case KittyKeyDown:
			keyPart = "down"
		case KittyKeyF1, KittyKeyF2, KittyKeyF3, KittyKeyF4, KittyKeyF5, KittyKeyF6,
			KittyKeyF7, KittyKeyF8, KittyKeyF9, KittyKeyF10, KittyKeyF11, KittyKeyF12:
			keyPart = fmt.Sprintf("f%d", keyCode-KittyKeyF1+1)
		default:
			if keyCode <= 0x10FFFF {
				keyPart = string(rune(keyCode))
			} else {
				keyPart = fmt.Sprintf("unknown(%d)", keyCode)
			}
		}
	}

	if len(modParts) > 0 {
		return strings.Join(modParts, "+") + "+" + keyPart
	}
	return keyPart
}

// IsPress returns true if this is a key press event (default for legacy keys)
func (k Key) IsPress() bool {
	return k.Type == KeyEventPress
}

// IsRepeat returns true if this is a key repeat event
func (k Key) IsRepeat() bool {
	return k.Type == KeyEventRepeat
}

// IsRelease returns true if this is a key release event
func (k Key) IsRelease() bool {
	return k.Type == KeyEventRelease
}

// HasModifier checks if a specific Kitty modifier is present
func (k Key) HasModifier(mod KeyModifiers) bool {
	return k.Mod&mod != 0
}

// HasCtrl returns true if Ctrl modifier is present
func (k Key) HasCtrl() bool {
	return k.HasModifier(KeyModCtrl)
}

// HasShift returns true if Shift modifier is present
func (k Key) HasShift() bool {
	return k.HasModifier(KeyModShift)
}

// HasAlt returns true if Alt modifier is present
func (k Key) HasAlt() bool {
	return k.HasModifier(KeyModAlt)
}

// HasSuper returns true if Super modifier is present
func (k Key) HasSuper() bool {
	return k.HasModifier(KeyModSuper)
}

// HasHyper returns true if Hyper modifier is present
func (k Key) HasHyper() bool {
	return k.HasModifier(KeyModHyper)
}

// HasMeta returns true if Meta modifier is present
func (k Key) HasMeta() bool {
	return k.HasModifier(KeyModMeta)
}
