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

	tea "github.com/charmbracelet/bubbletea"
)

// KeyMsg contains information about a keypress. KeyMsgs are always sent to
// the program's update function. There are a couple general patterns you could
// use to check for keypresses:
//
//	// Switch on the string representation of the key (shorter)
//	switch msg := msg.(type) {
//	case KeyMsg:
//	    switch msg.String() {
//	    case "enter":
//	        fmt.Println("you pressed enter!")
//	    case "a":
//	        fmt.Println("you pressed a!")
//	    }
//	}
//
//	// Switch on the key type (more foolproof)
//	switch msg := msg.(type) {
//	case KeyMsg:
//	    switch msg.Type {
//	    case KeyEnter:
//	        fmt.Println("you pressed enter!")
//	    case KeyRunes:
//	        switch string(msg.Runes) {
//	        case "a":
//	            fmt.Println("you pressed a!")
//	        }
//	    }
//	}
//
// Note that Key.Runes will always contain at least one character, so you can
// always safely call Key.Runes[0]. In most cases Key.Runes will only contain
// one character, though certain input method editors (most notably Chinese
// IMEs) can input multiple runes at once.
type KeyMsg Key

func (k KeyMsg) ToTea() tea.KeyMsg {
	return tea.KeyMsg{
		Type:  tea.KeyType(k.Type),
		Runes: k.Runes,
		Alt:   k.Alt,
	}
}

// String returns a string representation for a key message. It's safe (and
// encouraged) for use in key comparison.
func (k KeyMsg) String() (str string) {
	return Key(k).String()
}

// Key contains information about a keypress.
type Key struct {
	Type  KeyType
	Runes []rune
	Alt   bool

	// Kitty protocol enhanced fields (optional, nil means not using Kitty protocol)
	Kitty *KittyKey
}

// String returns a friendly string representation for a key. It's safe (and
// encouraged) for use in key comparison.
//
//	k := Key{Type: KeyEnter}
//	fmt.Println(k)
//	// Output: enter
func (k Key) String() (str string) {
	if k.Alt {
		str += "alt+"
	}
	if k.Type == KeyRunes {
		str += string(k.Runes)
		return str
	} else if s, ok := keyNames[k.Type]; ok {
		str += s
		return str
	}
	return ""
}

func (k Key) Bytes() (data []byte) {
	// If this key has Kitty protocol information, use it if appropriate
	if k.HasKittyProtocol() && k.Kitty != nil {
		// For now, always fall back to legacy bytes for compatibility
		// In the future, this could check terminal capabilities
		return k.legacyBytes()
	}
	return k.legacyBytes()
}

// legacyBytes returns the traditional byte encoding for a key
func (k Key) legacyBytes() (data []byte) {
	// This is the existing KeysToBytes logic but for a single key
	switch k.Type {
	case KeySpace:
		data = append(data, []byte(" ")...)
	case KeyRunes:
		if k.Alt {
			data = append(data, '\x1b')
		}
		data = append(data, []byte(string(k.Runes))...)
	default:
		if seq, ok := inverseSequences[keyLookup{
			Type: k.Type,
			Alt:  k.Alt,
		}]; ok {
			data = append(data, seq...)
			return data
		}

		// ESC is also used to indicate an alt+ key sequence,
		// so we can't just use a predefined sequence.
		if k.Type == keyESC {
			data = append(data, '\x1b')
			return data
		}
	}
	return data
}

// HasKittyProtocol returns true if this key was parsed using Kitty protocol
func (k Key) HasKittyProtocol() bool {
	return k.Kitty != nil
}

// IsPress returns true if this is a key press event (default for legacy keys)
func (k Key) IsPress() bool {
	if k.Kitty != nil {
		return k.Kitty.EventType == KittyKeyPress
	}
	return true // Legacy keys are always considered press events
}

// IsRepeat returns true if this is a key repeat event
func (k Key) IsRepeat() bool {
	if k.Kitty != nil {
		return k.Kitty.EventType == KittyKeyRepeat
	}
	return false
}

// IsRelease returns true if this is a key release event
func (k Key) IsRelease() bool {
	if k.Kitty != nil {
		return k.Kitty.EventType == KittyKeyRelease
	}
	return false
}

// HasModifier checks if a specific Kitty modifier is present
func (k Key) HasModifier(mod KittyModifiers) bool {
	if k.Kitty != nil {
		return k.Kitty.Modifiers&mod != 0
	}
	// For legacy keys, only check Alt
	if mod == KittyModAlt {
		return k.Alt
	}
	return false
}

// HasCtrl returns true if Ctrl modifier is present
func (k Key) HasCtrl() bool {
	return k.HasModifier(KittyModCtrl)
}

// HasShift returns true if Shift modifier is present
func (k Key) HasShift() bool {
	return k.HasModifier(KittyModShift)
}

// HasAlt returns true if Alt modifier is present
func (k Key) HasAlt() bool {
	return k.HasModifier(KittyModAlt) || k.Alt
}

// HasSuper returns true if Super modifier is present
func (k Key) HasSuper() bool {
	return k.HasModifier(KittyModSuper)
}

// HasHyper returns true if Hyper modifier is present
func (k Key) HasHyper() bool {
	return k.HasModifier(KittyModHyper)
}

// HasMeta returns true if Meta modifier is present
func (k Key) HasMeta() bool {
	return k.HasModifier(KittyModMeta)
}

// NewKittyKey creates a new Key from a KittyKey
func NewKittyKey(kittyKey KittyKey) Key {
	legacyKey := kittyKey.ToLegacyKey()
	legacyKey.Kitty = &kittyKey
	return legacyKey
}

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
// etc) into KeyMsg events. Unrecognized strings are represented as KeyRunes.
func KeysToMsg(keys ...string) (msgs []KeyMsg) {
	for _, key := range keys {
		var alt bool
		if strings.HasPrefix(key, "alt+") && len(key) > 4 {
			alt = true
			key = key[4:]
		}

		if _type, ok := keyRefs[key]; ok {
			msgs = append(msgs, KeyMsg{
				Type: _type,
				Alt:  alt,
			})
			continue
		}

		msgs = append(msgs, KeyMsg{
			Type:  KeyRunes,
			Runes: []rune(key),
			Alt:   alt,
		})
	}
	return
}

func KeysToBytes(keys ...KeyMsg) (data []byte, err error) {
	for _, key := range keys {
		switch key.Type {
		case KeySpace:
			data = append(data, []byte(" ")...)
		case KeyRunes:
			if key.Alt {
				data = append(data, '\x1b')
			}

			data = append(data, []byte(string(key.Runes))...)
		default:
			if seq, ok := inverseSequences[keyLookup{
				Type: key.Type,
				Alt:  key.Alt,
			}]; ok {
				data = append(data, []byte(seq)...)
				continue
			}

			// ESC is also used to indicate an alt+ key sequence,
			// so we can't just use a predefined sequence.
			if key.Type == keyESC {
				data = append(data, '\x1b')
				continue
			}

			err = fmt.Errorf("unknown key type %+v", key)
			return
		}
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
		kittyKey, width, err := ParseKittySequence(b)
		if err == nil {
			key := NewKittyKey(kittyKey)
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
		return i + 1, KeyMsg{Type: keyNUL, Alt: alt}
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
	// If we found at least one rune, we report the bunch of them as
	// a single KeyRunes or KeySpace event.
	if len(runes) > 0 {
		k := Key{Type: KeyRunes, Runes: runes, Alt: alt}
		if len(runes) == 1 && runes[0] == ' ' {
			k.Type = KeySpace
		}
		return i, KeyMsg(k)
	}

	// We didn't find an escape sequence, nor a valid rune. Was this a
	// lone escape character at the end of the input?
	if alt && len(b) == 1 {
		return 1, KeyMsg(Key{Type: KeyEscape})
	}

	// The character at the current position is neither an escape
	// sequence, a valid rune start or a sole escape character. Report
	// it as an invalid byte.
	return 1, unknownInputByteMsg(b[0])
}
