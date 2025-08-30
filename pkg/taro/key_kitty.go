package taro

import (
	"fmt"
	"strconv"
	"strings"
)

// KittyKeyEventType represents the type of key event (press/repeat/release)
type KittyKeyEventType int

const (
	KittyKeyPress KittyKeyEventType = iota
	KittyKeyRepeat
	KittyKeyRelease
)

func (k KittyKeyEventType) String() string {
	switch k {
	case KittyKeyPress:
		return "press"
	case KittyKeyRepeat:
		return "repeat"
	case KittyKeyRelease:
		return "release"
	default:
		return "unknown"
	}
}

// KittyModifiers represents Kitty protocol modifier flags
type KittyModifiers int

const (
	KittyModShift KittyModifiers = 1 << iota
	KittyModAlt
	KittyModCtrl
	KittyModSuper
	KittyModHyper
	KittyModMeta
	KittyModCapsLock
	KittyModNumLock
)

// KittyProgressiveFlags represents progressive enhancement flags
type KittyProgressiveFlags int

const (
	KittyDisambiguateEscape KittyProgressiveFlags = 1 << iota
	KittyReportEventTypes
	KittyReportAlternateKeys
	KittyReportAllKeys
	KittyReportAssociatedText
)

// Kitty protocol special keys using Unicode Private Use Area
const (
	KittyKeyEscape    = 0x1B
	KittyKeyEnter     = 0x0D
	KittyKeyTab       = 0x09
	KittyKeyBackspace = 0x7F
	KittyKeyInsert    = 0xE000 + 2
	KittyKeyDelete    = 0xE000 + 3
	KittyKeyHome      = 0xE000 + 1
	KittyKeyEnd       = 0xE000 + 4
	KittyKeyPageUp    = 0xE000 + 5
	KittyKeyPageDown  = 0xE000 + 6
	KittyKeyLeft      = 0xE000 + 68
	KittyKeyUp        = 0xE000 + 65
	KittyKeyRight     = 0xE000 + 67
	KittyKeyDown      = 0xE000 + 66
	KittyKeyF1        = 0xE000 + 112
	KittyKeyF2        = 0xE000 + 113
	KittyKeyF3        = 0xE000 + 114
	KittyKeyF4        = 0xE000 + 115
	KittyKeyF5        = 0xE000 + 116
	KittyKeyF6        = 0xE000 + 117
	KittyKeyF7        = 0xE000 + 118
	KittyKeyF8        = 0xE000 + 119
	KittyKeyF9        = 0xE000 + 120
	KittyKeyF10       = 0xE000 + 121
	KittyKeyF11       = 0xE000 + 122
	KittyKeyF12       = 0xE000 + 123
)

// KittyKey represents an enhanced key with Kitty protocol information
type KittyKey struct {
	KeyCode    int               // Unicode key code or Kitty special key
	Modifiers  KittyModifiers    // Combined modifier flags
	EventType  KittyKeyEventType // Press/repeat/release
	BaseKey    rune              // Base layout key (without shift)
	ShiftedKey rune              // Shifted key value
	Text       string            // Associated text (if any)
}

// ToLegacyKey converts a KittyKey to the legacy Key format for compatibility
func (k KittyKey) ToLegacyKey() Key {
	key := Key{
		Alt: k.Modifiers&KittyModAlt != 0,
	}

	// Map Kitty key codes to legacy KeyType
	switch k.KeyCode {
	case KittyKeyEscape:
		key.Type = KeyEscape
	case KittyKeyEnter:
		key.Type = KeyEnter
	case KittyKeyTab:
		key.Type = KeyTab
	case KittyKeyBackspace:
		key.Type = KeyBackspace
	case KittyKeyInsert:
		key.Type = KeyInsert
	case KittyKeyDelete:
		key.Type = KeyDelete
	case KittyKeyHome:
		if k.Modifiers&KittyModCtrl != 0 && k.Modifiers&KittyModShift != 0 {
			key.Type = KeyCtrlShiftHome
		} else if k.Modifiers&KittyModCtrl != 0 {
			key.Type = KeyCtrlHome
		} else if k.Modifiers&KittyModShift != 0 {
			key.Type = KeyShiftHome
		} else {
			key.Type = KeyHome
		}
	case KittyKeyEnd:
		if k.Modifiers&KittyModCtrl != 0 && k.Modifiers&KittyModShift != 0 {
			key.Type = KeyCtrlShiftEnd
		} else if k.Modifiers&KittyModCtrl != 0 {
			key.Type = KeyCtrlEnd
		} else if k.Modifiers&KittyModShift != 0 {
			key.Type = KeyShiftEnd
		} else {
			key.Type = KeyEnd
		}
	case KittyKeyPageUp:
		if k.Modifiers&KittyModCtrl != 0 {
			key.Type = KeyCtrlPgUp
		} else {
			key.Type = KeyPgUp
		}
	case KittyKeyPageDown:
		if k.Modifiers&KittyModCtrl != 0 {
			key.Type = KeyCtrlPgDown
		} else {
			key.Type = KeyPgDown
		}
	case KittyKeyLeft:
		if k.Modifiers&KittyModCtrl != 0 && k.Modifiers&KittyModShift != 0 {
			key.Type = KeyCtrlShiftLeft
		} else if k.Modifiers&KittyModCtrl != 0 {
			key.Type = KeyCtrlLeft
		} else if k.Modifiers&KittyModShift != 0 {
			key.Type = KeyShiftLeft
		} else {
			key.Type = KeyLeft
		}
	case KittyKeyUp:
		if k.Modifiers&KittyModCtrl != 0 && k.Modifiers&KittyModShift != 0 {
			key.Type = KeyCtrlShiftUp
		} else if k.Modifiers&KittyModCtrl != 0 {
			key.Type = KeyCtrlUp
		} else if k.Modifiers&KittyModShift != 0 {
			key.Type = KeyShiftUp
		} else {
			key.Type = KeyUp
		}
	case KittyKeyRight:
		if k.Modifiers&KittyModCtrl != 0 && k.Modifiers&KittyModShift != 0 {
			key.Type = KeyCtrlShiftRight
		} else if k.Modifiers&KittyModCtrl != 0 {
			key.Type = KeyCtrlRight
		} else if k.Modifiers&KittyModShift != 0 {
			key.Type = KeyShiftRight
		} else {
			key.Type = KeyRight
		}
	case KittyKeyDown:
		if k.Modifiers&KittyModCtrl != 0 && k.Modifiers&KittyModShift != 0 {
			key.Type = KeyCtrlShiftDown
		} else if k.Modifiers&KittyModCtrl != 0 {
			key.Type = KeyCtrlDown
		} else if k.Modifiers&KittyModShift != 0 {
			key.Type = KeyShiftDown
		} else {
			key.Type = KeyDown
		}
	case KittyKeyF1:
		key.Type = KeyF1
	case KittyKeyF2:
		key.Type = KeyF2
	case KittyKeyF3:
		key.Type = KeyF3
	case KittyKeyF4:
		key.Type = KeyF4
	case KittyKeyF5:
		key.Type = KeyF5
	case KittyKeyF6:
		key.Type = KeyF6
	case KittyKeyF7:
		key.Type = KeyF7
	case KittyKeyF8:
		key.Type = KeyF8
	case KittyKeyF9:
		key.Type = KeyF9
	case KittyKeyF10:
		key.Type = KeyF10
	case KittyKeyF11:
		key.Type = KeyF11
	case KittyKeyF12:
		key.Type = KeyF12
	default:
		// Regular Unicode character
		if k.KeyCode <= 0x10FFFF {
			key.Type = KeyRunes
			key.Runes = []rune{rune(k.KeyCode)}
		}
	}

	return key
}

// String returns a human-readable representation of the KittyKey
func (k KittyKey) String() string {
	var modParts []string

	if k.Modifiers&KittyModSuper != 0 {
		modParts = append(modParts, "super")
	}
	if k.Modifiers&KittyModHyper != 0 {
		modParts = append(modParts, "hyper")
	}
	if k.Modifiers&KittyModMeta != 0 {
		modParts = append(modParts, "meta")
	}
	if k.Modifiers&KittyModCtrl != 0 {
		modParts = append(modParts, "ctrl")
	}
	if k.Modifiers&KittyModAlt != 0 {
		modParts = append(modParts, "alt")
	}
	if k.Modifiers&KittyModShift != 0 {
		modParts = append(modParts, "shift")
	}

	var keyPart string
	switch k.KeyCode {
	case KittyKeyEscape:
		keyPart = "esc"
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
		keyPart = fmt.Sprintf("f%d", k.KeyCode-KittyKeyF1+1)
	default:
		if k.KeyCode <= 0x10FFFF {
			keyPart = string(rune(k.KeyCode))
		} else {
			keyPart = fmt.Sprintf("unknown(%d)", k.KeyCode)
		}
	}

	if len(modParts) > 0 {
		return strings.Join(modParts, "+") + "+" + keyPart
	}
	return keyPart
}

// IsKittySequence checks if the byte sequence might be a Kitty protocol sequence
// Kitty sequences follow the pattern: ESC [ {keycode} [; {modifiers}] u
func IsKittySequence(b []byte) bool {
	if len(b) < 4 {
		return false
	}

	// Must start with ESC [
	if b[0] != '\x1b' || b[1] != '[' {
		return false
	}

	// Must end with 'u'
	if b[len(b)-1] != 'u' {
		return false
	}

	// Check if it's a keyboard protocol command (starts with >, <, or ?)
	if len(b) > 2 && (b[2] == '>' || b[2] == '<' || b[2] == '?') {
		return false
	}

	// Content between '[' and 'u' must start with a digit (keycode)
	content := string(b[2 : len(b)-1])
	if len(content) == 0 {
		return false
	}

	// First character must be a digit
	return content[0] >= '0' && content[0] <= '9'
}

// ParseKittySequence parses a Kitty protocol key sequence
// Format: ESC [ {keycode} [; {modifiers} [; {event_type} [; {text}]]] u
func ParseKittySequence(b []byte) (KittyKey, int, error) {
	if !IsKittySequence(b) {
		return KittyKey{}, 0, fmt.Errorf("not a valid Kitty sequence")
	}
	
	// Extract the content between '[' and 'u'
	content := string(b[2 : len(b)-1])
	parts := strings.Split(content, ";")
	
	if len(parts) == 0 {
		return KittyKey{}, 0, fmt.Errorf("empty key sequence")
	}
	
	// Parse keycode (required)
	keycode, err := strconv.Atoi(parts[0])
	if err != nil {
		return KittyKey{}, 0, fmt.Errorf("invalid keycode: %v", err)
	}
	
	key := KittyKey{
		KeyCode:   keycode,
		EventType: KittyKeyPress, // Default to press event
	}
	
	// Parse modifiers (optional)
	if len(parts) > 1 && parts[1] != "" {
		modifierBits, err := strconv.Atoi(parts[1])
		if err != nil {
			return KittyKey{}, 0, fmt.Errorf("invalid modifiers: %v", err)
		}
		key.Modifiers = KittyModifiers(modifierBits)
	}
	
	// Parse event type (optional)
	if len(parts) > 2 && parts[2] != "" {
		eventTypeBits, err := strconv.Atoi(parts[2])
		if err != nil {
			return KittyKey{}, 0, fmt.Errorf("invalid event type: %v", err)
		}
		
		// Map event type bits to KittyKeyEventType
		switch eventTypeBits {
		case 0:
			key.EventType = KittyKeyPress
		case 1:
			key.EventType = KittyKeyRepeat
		case 2:
			key.EventType = KittyKeyRelease
		default:
			return KittyKey{}, 0, fmt.Errorf("unknown event type: %d", eventTypeBits)
		}
	}
	
	// Parse associated text (optional)
	if len(parts) > 3 && parts[3] != "" {
		key.Text = parts[3]
	}
	
	return key, len(b), nil
}


// GenerateKittyEnableSequence generates the escape sequence to enable Kitty protocol
func GenerateKittyEnableSequence(flags KittyProgressiveFlags) string {
	return fmt.Sprintf("\x1b[>%d;1u", int(flags))
}

// GenerateKittyDisableSequence generates the escape sequence to disable Kitty protocol
func GenerateKittyDisableSequence() string {
	return "\x1b[<u"
}

// Bytes returns the byte representation of the KittyKey using Kitty protocol
func (k KittyKey) Bytes() []byte {
	return []byte(k.kittySequence())
}

// kittySequence generates the Kitty protocol sequence for this key
func (k KittyKey) kittySequence() string {
	keycode := k.KeyCode
	modifiers := int(k.Modifiers)
	eventType := int(k.EventType)

	if eventType != 0 || k.Text != "" {
		// Extended format with event type and text
		if k.Text != "" {
			return fmt.Sprintf("\x1b[%d;%d;%d;%su", keycode, modifiers, eventType, k.Text)
		} else {
			return fmt.Sprintf("\x1b[%d;%d;%du", keycode, modifiers, eventType)
		}
	} else if modifiers != 0 {
		// Standard format with modifiers
		return fmt.Sprintf("\x1b[%d;%du", keycode, modifiers)
	} else {
		// Minimal format
		return fmt.Sprintf("\x1b[%du", keycode)
	}
}
