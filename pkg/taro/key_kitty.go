package taro

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/cfoust/cy/pkg/emu"
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

// kittySequence generates the Kitty protocol sequence for this key
func (k Key) kittySequence(protocol emu.KeyProtocol) string {
	// TODO(cfoust): 08/30/25 handle KittyKeyRunes

	if len(k.Runes) == 0 {
		return ""
	}
	keycode := k.Runes[0]
	modifiers := int(k.Modifiers)
	eventType := int(k.Type)

	// Check if event types should be reported
	reportEventTypes := protocol&emu.KeyReportEventTypes != 0

	// Check if associated text should be reported
	reportText := protocol&emu.KeyReportAssociatedText != 0

	// Determine if we should include event type in output
	includeEventType := reportEventTypes && eventType != 0

	// Determine if we should include text in output
	// Extract printable characters from Runes field for text support
	var text string
	if reportText && len(k.Runes) > 0 {
		var printableRunes []rune
		for _, r := range k.Runes {
			// Include only printable characters (not special KittyKey constants)
			if r <= 0x10FFFF && r > 31 && r != 127 && r < 0xE000 {
				printableRunes = append(printableRunes, r)
			}
		}
		if len(printableRunes) > 0 {
			text = string(printableRunes)
		}
	}
	includeText := text != ""

	if includeEventType || includeText {
		// Extended format with event type and/or text
		if includeText {
			return fmt.Sprintf(
				"\x1b[%d;%d;%d;%su",
				keycode,
				modifiers,
				eventType,
				text,
			)
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
func ParseKittySequence(b []byte) (Key, int, error) {
	if !IsKittySequence(b) {
		return Key{}, 0, fmt.Errorf("not a valid Kitty sequence")
	}

	// Extract the content between '[' and 'u'
	content := string(b[2 : len(b)-1])
	parts := strings.Split(content, ";")

	if len(parts) == 0 {
		return Key{}, 0, fmt.Errorf("empty key sequence")
	}

	// Parse keycode (required)
	keycode, err := strconv.Atoi(parts[0])
	if err != nil {
		return Key{}, 0, fmt.Errorf("invalid keycode: %v", err)
	}

	key := Key{
		Runes: []rune{rune(keycode)},
		Type:  KeyEventPress, // Default to press event
	}

	// Parse modifiers (optional)
	if len(parts) > 1 && parts[1] != "" {
		modifierBits, err := strconv.Atoi(parts[1])
		if err != nil {
			return Key{}, 0, fmt.Errorf("invalid modifiers: %v", err)
		}
		key.Modifiers = KeyModifiers(modifierBits)
	}

	// Parse event type (optional)
	if len(parts) > 2 && parts[2] != "" {
		eventTypeBits, err := strconv.Atoi(parts[2])
		if err != nil {
			return Key{}, 0, fmt.Errorf("invalid event type: %v", err)
		}

		// Map event type bits to KittyKeyEventType
		switch eventTypeBits {
		case 0:
			key.Type = KeyEventPress
		case 1:
			key.Type = KeyEventRepeat
		case 2:
			key.Type = KeyEventRelease
		default:
			return Key{}, 0, fmt.Errorf("unknown event type: %d", eventTypeBits)
		}
	}

	// Parse associated text (optional)
	if len(parts) > 3 && parts[3] != "" {
		// For now, text is handled implicitly through the Runes field
		// The text would be stored in the Runes field as printable characters
		// during key creation. This parsing is primarily for validation.
		// TODO: Consider if we need to store text separately or append to Runes
	}

	return key, len(b), nil
}

// GenerateKittyEnableSequence generates the escape sequence to enable Kitty protocol
func GenerateKittyEnableSequence(flags emu.KeyProtocol) string {
	return fmt.Sprintf("\x1b[>%d;1u", int(flags))
}

// GenerateKittyDisableSequence generates the escape sequence to disable Kitty protocol
func GenerateKittyDisableSequence() string {
	return "\x1b[<u"
}
