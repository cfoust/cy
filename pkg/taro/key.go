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

package taro

import (
	"fmt"
	"io"
	"regexp"
	"strings"
	"unicode/utf8"

	"github.com/cfoust/cy/pkg/emu"

	tea "github.com/charmbracelet/bubbletea"
)

type KeyMsg Key

func (k KeyMsg) ToTea() tea.KeyMsg {
	key := Key(k)
	legacyType := key.toLegacyKeyType()

	var runes []rune
	if legacyType == 0 && len(key.Runes) > 0 && key.Runes[0] <= 0x10FFFF &&
		key.Runes[0] > 31 &&
		key.Runes[0] != 127 {
		runes = []rune{rune(key.Runes[0])}
		legacyType = KeyRunes
	}

	return tea.KeyMsg{
		Type:  tea.KeyType(legacyType),
		Runes: runes,
		Alt:   key.Modifiers&KeyModAlt != 0,
	}
}

// String returns a string representation for a key message. It's safe (and
// encouraged) for use in key comparison.
func (k KeyMsg) String() (str string) {
	return Key(k).String()
}

// Key contains information about a keypress using Kitty protocol structure.
type Key struct {
	Runes     []rune       // Can also contain KittyKeys
	Type      KeyEventType // Press/repeat/release
	Modifiers KeyModifiers // Combined modifier flags
}

// String returns a friendly string representation for a key. It's safe (and
// encouraged) for use in key comparison.
//
//	k := Key{KeyCode: KittyKeyEnter}
//	fmt.Println(k)
//	// Output: enter
func (k Key) String() (str string) {
	var modParts []string

	if k.Modifiers&KeyModSuper != 0 {
		modParts = append(modParts, "super")
	}
	if k.Modifiers&KeyModHyper != 0 {
		modParts = append(modParts, "hyper")
	}
	if k.Modifiers&KeyModMeta != 0 {
		modParts = append(modParts, "meta")
	}
	if k.Modifiers&KeyModCtrl != 0 {
		modParts = append(modParts, "ctrl")
	}
	if k.Modifiers&KeyModAlt != 0 {
		modParts = append(modParts, "alt")
	}
	if k.Modifiers&KeyModShift != 0 {
		modParts = append(modParts, "shift")
	}

	var keyPart string
	if len(k.Runes) == 0 {
		keyPart = "unknown"
	} else {
		keyCode := k.Runes[0]
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

func (k Key) Bytes(protocol emu.KeyProtocol) (data []byte) {
	// Use Kitty protocol if enabled
	if protocol > emu.KeyLegacy {
		return []byte(k.kittySequence(protocol))
	}
	return k.legacyBytes()
}

// legacyBytes returns the traditional byte encoding for a key
func (k Key) legacyBytes() (data []byte) {
	// Convert Kitty-based key to legacy format
	alt := k.Modifiers&KeyModAlt != 0

	if len(k.Runes) == 0 {
		return data
	}

	// If we have multiple runes, they're likely regular Unicode characters
	if len(k.Runes) > 1 {
		if alt {
			data = append(data, '\x1b')
		}
		data = append(data, []byte(string(k.Runes))...)
		return data
	}

	keyCode := k.Runes[0]

	// Handle space specially
	if keyCode == ' ' {
		data = append(data, []byte(" ")...)
		return data
	}

	// Handle regular Unicode characters
	if keyCode <= 0x10FFFF && keyCode > 31 && keyCode != 127 {
		if alt {
			data = append(data, '\x1b')
		}
		data = append(data, []byte(string(rune(keyCode)))...)
		return data
	}

	// Convert Kitty special keys to legacy KeyType and use inverse sequences
	legacyType := k.toLegacyKeyType()
	if legacyType != 0 {
		if seq, ok := inverseSequences[keyLookup{
			Type: legacyType,
			Alt:  alt,
		}]; ok {
			data = append(data, seq...)
			return data
		}

		// ESC is special case
		if legacyType == keyESC {
			data = append(data, '\x1b')
			return data
		}
	}

	return data
}

// toLegacyKeyType converts a Kitty key code to legacy KeyType
func (k Key) toLegacyKeyType() KeyType {
	if len(k.Runes) == 0 {
		return 0
	}
	keyCode := k.Runes[0]
	switch keyCode {
	case KittyKeyEscape:
		return KeyEscape
	case KittyKeyEnter:
		return KeyEnter
	case KittyKeyTab:
		return KeyTab
	case KittyKeyBackspace:
		return KeyBackspace
	case KittyKeyInsert:
		return KeyInsert
	case KittyKeyDelete:
		return KeyDelete
	case KittyKeyHome:
		if k.Modifiers&KeyModCtrl != 0 && k.Modifiers&KeyModShift != 0 {
			return KeyCtrlShiftHome
		} else if k.Modifiers&KeyModCtrl != 0 {
			return KeyCtrlHome
		} else if k.Modifiers&KeyModShift != 0 {
			return KeyShiftHome
		} else {
			return KeyHome
		}
	case KittyKeyEnd:
		if k.Modifiers&KeyModCtrl != 0 && k.Modifiers&KeyModShift != 0 {
			return KeyCtrlShiftEnd
		} else if k.Modifiers&KeyModCtrl != 0 {
			return KeyCtrlEnd
		} else if k.Modifiers&KeyModShift != 0 {
			return KeyShiftEnd
		} else {
			return KeyEnd
		}
	case KittyKeyPageUp:
		if k.Modifiers&KeyModCtrl != 0 {
			return KeyCtrlPgUp
		} else {
			return KeyPgUp
		}
	case KittyKeyPageDown:
		if k.Modifiers&KeyModCtrl != 0 {
			return KeyCtrlPgDown
		} else {
			return KeyPgDown
		}
	case KittyKeyLeft:
		if k.Modifiers&KeyModCtrl != 0 && k.Modifiers&KeyModShift != 0 {
			return KeyCtrlShiftLeft
		} else if k.Modifiers&KeyModCtrl != 0 {
			return KeyCtrlLeft
		} else if k.Modifiers&KeyModShift != 0 {
			return KeyShiftLeft
		} else {
			return KeyLeft
		}
	case KittyKeyUp:
		if k.Modifiers&KeyModCtrl != 0 && k.Modifiers&KeyModShift != 0 {
			return KeyCtrlShiftUp
		} else if k.Modifiers&KeyModCtrl != 0 {
			return KeyCtrlUp
		} else if k.Modifiers&KeyModShift != 0 {
			return KeyShiftUp
		} else {
			return KeyUp
		}
	case KittyKeyRight:
		if k.Modifiers&KeyModCtrl != 0 && k.Modifiers&KeyModShift != 0 {
			return KeyCtrlShiftRight
		} else if k.Modifiers&KeyModCtrl != 0 {
			return KeyCtrlRight
		} else if k.Modifiers&KeyModShift != 0 {
			return KeyShiftRight
		} else {
			return KeyRight
		}
	case KittyKeyDown:
		if k.Modifiers&KeyModCtrl != 0 && k.Modifiers&KeyModShift != 0 {
			return KeyCtrlShiftDown
		} else if k.Modifiers&KeyModCtrl != 0 {
			return KeyCtrlDown
		} else if k.Modifiers&KeyModShift != 0 {
			return KeyShiftDown
		} else {
			return KeyDown
		}
	case KittyKeyF1:
		return KeyF1
	case KittyKeyF2:
		return KeyF2
	case KittyKeyF3:
		return KeyF3
	case KittyKeyF4:
		return KeyF4
	case KittyKeyF5:
		return KeyF5
	case KittyKeyF6:
		return KeyF6
	case KittyKeyF7:
		return KeyF7
	case KittyKeyF8:
		return KeyF8
	case KittyKeyF9:
		return KeyF9
	case KittyKeyF10:
		return KeyF10
	case KittyKeyF11:
		return KeyF11
	case KittyKeyF12:
		return KeyF12
	default:
		return 0
	}
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
	return k.Modifiers&mod != 0
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

// Runes returns the key as a slice of runes for compatibility with legacy code
//func (k Key) Runes() []rune {
//// Only return runes for regular Unicode characters (not special keys)
//if k.KeyCode <= 0x10FFFF && k.KeyCode > 31 && k.KeyCode != 127 {
//return []rune{rune(k.KeyCode)}
//}
//return nil
//}

// KeyType indicates the key pressed, such as KeyEnter or KeyBreak or KeyCtrlC.
// All other keys will be type KeyRunes. To get the rune value, check the Rune
// method on a Key struct, or use the Key.String() method:
//
//	k := Key{Type: KeyRunes, Runes: []rune{'a'}, Alt: true}
//	if k.Type == KeyRunes {
//
//	    fmt.Println(k.Runes)
//	    // Output: a
//
//	    fmt.Println(k.String())
//	    // Output: alt+a
//
//	}
type KeyType int

func (k KeyType) String() (str string) {
	if s, ok := keyNames[k]; ok {
		return s
	}
	return ""
}

// KeysToMsg translates human-readable key specifiers (such as "ctrl+a", "up",
// etc) into KeyMsg events. Unrecognized strings are represented as regular characters.
func KeysToMsg(keys ...string) (msgs []KeyMsg) {
	for _, key := range keys {
		var modifiers KeyModifiers

		// Parse modifiers
		for {
			if strings.HasPrefix(key, "alt+") && len(key) > 4 {
				modifiers |= KeyModAlt
				key = key[4:]
			} else if strings.HasPrefix(key, "ctrl+") && len(key) > 5 {
				modifiers |= KeyModCtrl
				key = key[5:]
			} else if strings.HasPrefix(key, "shift+") && len(key) > 6 {
				modifiers |= KeyModShift
				key = key[6:]
			} else if strings.HasPrefix(key, "super+") && len(key) > 6 {
				modifiers |= KeyModSuper
				key = key[6:]
			} else if strings.HasPrefix(key, "hyper+") && len(key) > 6 {
				modifiers |= KeyModHyper
				key = key[6:]
			} else if strings.HasPrefix(key, "meta+") && len(key) > 5 {
				modifiers |= KeyModMeta
				key = key[5:]
			} else {
				break
			}
		}

		// Map key names to Kitty key codes
		var keyCode rune = -1
		switch key {
		case "escape", "esc":
			keyCode = KittyKeyEscape
		case "enter":
			keyCode = KittyKeyEnter
		case "tab":
			keyCode = KittyKeyTab
		case "backspace":
			keyCode = KittyKeyBackspace
		case "insert":
			keyCode = KittyKeyInsert
		case "delete":
			keyCode = KittyKeyDelete
		case "home":
			keyCode = KittyKeyHome
		case "end":
			keyCode = KittyKeyEnd
		case "pgup":
			keyCode = KittyKeyPageUp
		case "pgdown":
			keyCode = KittyKeyPageDown
		case "left":
			keyCode = KittyKeyLeft
		case "up":
			keyCode = KittyKeyUp
		case "right":
			keyCode = KittyKeyRight
		case "down":
			keyCode = KittyKeyDown
		case "f1":
			keyCode = KittyKeyF1
		case "f2":
			keyCode = KittyKeyF2
		case "f3":
			keyCode = KittyKeyF3
		case "f4":
			keyCode = KittyKeyF4
		case "f5":
			keyCode = KittyKeyF5
		case "f6":
			keyCode = KittyKeyF6
		case "f7":
			keyCode = KittyKeyF7
		case "f8":
			keyCode = KittyKeyF8
		case "f9":
			keyCode = KittyKeyF9
		case "f10":
			keyCode = KittyKeyF10
		case "f11":
			keyCode = KittyKeyF11
		case "f12":
			keyCode = KittyKeyF12
		case "space":
			keyCode = ' '
		}

		runes := []rune{keyCode}

		// For unrecognized keys, use the first character
		if keyCode == -1 && len(key) > 0 {
			runes = []rune(key)
		}

		msgs = append(msgs, KeyMsg{
			Runes:     runes,
			Modifiers: modifiers,
			Type:      KeyEventPress,
		})
	}
	return
}

func KeysToBytes(keys ...KeyMsg) (data []byte, err error) {
	return KeysToBytesWithProtocol(emu.KeyLegacy, keys...)
}

// KeysToBytesWithProtocol converts KeyMsgs to bytes using the specified protocol
func KeysToBytesWithProtocol(
	protocol emu.KeyProtocol,
	keys ...KeyMsg,
) (data []byte, err error) {
	for _, key := range keys {
		keyBytes := Key(key).Bytes(protocol)
		data = append(data, keyBytes...)
	}
	return
}

// unknownInputByteMsg is reported by the input reader when an invalid
// utf-8 byte is detected on the input. Currently, it is not handled
// further by bubbletea. However, having this event makes it possible
// to troubleshoot invalid inputs.
type unknownInputByteMsg byte

func (u unknownInputByteMsg) String() string {
	return fmt.Sprintf("?%#02x?", int(u))
}

// unknownCSISequenceMsg is reported by the input reader when an
// unrecognized CSI sequence is detected on the input. Currently, it
// is not handled further by bubbletea. However, having this event
// makes it possible to troubleshoot invalid inputs.
type unknownCSISequenceMsg []byte

func (u unknownCSISequenceMsg) String() string {
	return fmt.Sprintf("?CSI%+v?", []byte(u)[2:])
}

var spaceRunes = []rune{' '}

// readInputs reads keypress and mouse inputs from a TTY and produces messages
// containing information about the key or mouse events accordingly.
func readInputs(input io.Reader) (msgs []Msg, err error) {
	var buf [256]byte

	// Read and block.
	numBytes, err := input.Read(buf[:])
	if err != nil {
		return nil, err
	}
	b := buf[:numBytes]

	var i, w int
	for i = 0; i < len(b); i += w {
		var msg Msg
		w, msg = DetectOneMsg(b[i:])
		msgs = append(msgs, msg)
	}

	return
}

var unknownCSIRe = regexp.MustCompile(
	`^\x1b\[[\x30-\x3f]*[\x20-\x2f]*[\x40-\x7e]`,
)

func isMouseEvent(b []byte) bool {
	return len(b) >= 6 && b[0] == '\x1b' && b[1] == '[' && b[2] == 'M'
}

func DetectOneMsg(b []byte) (w int, msg Msg) {
	// Try Kitty protocol sequence first
	if IsKittySequence(b) {
		key, width, err := ParseKittySequence(b)
		if err == nil {
			return width, KeyMsg(key)
		}
	}

	// Detect mouse events.
	if isMouseEvent(b) {
		return 6, MouseMsg(parseX10MouseEvent(b))
	}

	// Detect escape sequence and control characters other than NUL,
	// possibly with an escape character in front to mark the Alt
	// modifier.
	var foundSeq bool
	foundSeq, w, msg = detectSequence(b)
	if foundSeq {
		return
	}

	// No non-NUL control character or escape sequence.
	// If we are seeing at least an escape character, remember it for later below.
	alt := false
	i := 0
	if b[0] == '\x1b' {
		alt = true
		i++
	}

	// Are we seeing a standalone NUL? This is not handled by detectSequence().
	if i < len(b) && b[i] == 0 {
		modifiers := KeyModifiers(0)
		if alt {
			modifiers |= KeyModAlt
		}
		return i + 1, KeyMsg{
			Runes:     []rune{0},
			Modifiers: modifiers,
			Type:      KeyEventPress,
		}
	}

	// Find the longest sequence of runes that are not control
	// characters from this point.
	var runes []rune
	for rw := 0; i < len(b); i += rw {
		var r rune
		r, rw = utf8.DecodeRune(b[i:])
		if r == utf8.RuneError || r <= rune(keyUS) || r == rune(keyDEL) ||
			r == ' ' {
			// Rune errors are handled below; control characters and spaces will
			// be handled by detectSequence in the next call to detectOneMsg.
			break
		}
		runes = append(runes, r)
		if alt {
			// We only support a single rune after an escape alt modifier.
			i += rw
			break
		}
	}
	if len(runes) > 0 {
		modifiers := KeyModifiers(0)
		if alt {
			modifiers |= KeyModAlt
		}

		k := Key{
			Runes:     runes,
			Modifiers: modifiers,
			Type:      KeyEventPress,
		}
		return i, KeyMsg(k)
	}

	// We didn't find an escape sequence, nor a valid rune. Was this a
	// lone escape character at the end of the input?
	if alt && len(b) == 1 {
		return 1, KeyMsg(Key{
			Runes: []rune{KittyKeyEscape},
			Type:  KeyEventPress,
		})
	}

	// The character at the current position is neither an escape
	// sequence, a valid rune start or a sole escape character. Report
	// it as an invalid byte.
	return 1, unknownInputByteMsg(b[0])
}
