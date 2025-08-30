package taro

import (
	"testing"
)

func TestIsKittySequence(t *testing.T) {
	tests := []struct {
		name     string
		input    []byte
		expected bool
	}{
		{"Valid minimal sequence", []byte("\x1b[65u"), true},
		{"Valid sequence with modifiers", []byte("\x1b[65;2u"), true},
		{"Valid complex sequence", []byte("\x1b[65;2;1;textpu"), true},
		{"Too short", []byte("\x1b[u"), false},
		{"Wrong start", []byte("abc[65u"), false},
		{"Wrong terminator", []byte("\x1b[65a"), false},
		{"No semicolon in complex", []byte("\x1b[65u"), true}, // This should still be valid
		{"Protocol command", []byte("\x1b[>1;1u"), false},     // These are protocol commands, not key sequences
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := IsKittySequence(tt.input)
			if result != tt.expected {
				t.Errorf("IsKittySequence(%q) = %v, want %v", tt.input, result, tt.expected)
			}
		})
	}
}

func TestParseKittySequence(t *testing.T) {
	tests := []struct {
		name          string
		input         []byte
		expectedKey   int
		expectedMod   KeyModifiers
		expectedEvent KittyKeyEventType
		expectedText  string
		expectedWidth int
		shouldError   bool
	}{
		{
			name:          "Simple key",
			input:         []byte("\x1b[65u"),
			expectedKey:   65, // 'A'
			expectedMod:   0,
			expectedEvent: KittyKeyPress,
			expectedWidth: 5,
			shouldError:   false,
		},
		{
			name:          "Key with modifiers",
			input:         []byte("\x1b[65;2u"),
			expectedKey:   65,
			expectedMod:   KeyModifiers(2), // Shift
			expectedEvent: KittyKeyPress,
			expectedWidth: 7,
			shouldError:   false,
		},
		{
			name:          "Key press event (explicit)",
			input:         []byte("\x1b[65;2;0u"),
			expectedKey:   65,
			expectedMod:   KeyModifiers(2),
			expectedEvent: KittyKeyPress,
			expectedWidth: 9,
			shouldError:   false,
		},
		{
			name:          "Key repeat event",
			input:         []byte("\x1b[65;2;1u"),
			expectedKey:   65,
			expectedMod:   KeyModifiers(2),
			expectedEvent: KittyKeyRepeat,
			expectedWidth: 9,
			shouldError:   false,
		},
		{
			name:          "Key release event",
			input:         []byte("\x1b[65;2;2u"),
			expectedKey:   65,
			expectedMod:   KeyModifiers(2),
			expectedEvent: KittyKeyRelease,
			expectedWidth: 9,
			shouldError:   false,
		},
		{
			name:          "Key with text",
			input:         []byte("\x1b[65;0;0;textu"),
			expectedKey:   65,
			expectedMod:   0,
			expectedEvent: KittyKeyPress,
			expectedText:  "text",
			expectedWidth: 14,
			shouldError:   false,
		},
		{
			name:          "Key repeat with no modifiers",
			input:         []byte("\x1b[97;;1u"),
			expectedKey:   97, // 'a'
			expectedMod:   0,
			expectedEvent: KittyKeyRepeat,
			expectedWidth: 8,
			shouldError:   false,
		},
		{
			name:        "Invalid event type",
			input:       []byte("\x1b[65;2;5u"),
			shouldError: true,
		},
		{
			name:        "Invalid sequence",
			input:       []byte("\x1b[abc;2u"),
			shouldError: true,
		},
		{
			name:        "Non-Kitty sequence",
			input:       []byte("\x1b[A"),
			shouldError: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			key, width, err := ParseKittySequence(tt.input)
			if tt.shouldError {
				if err == nil {
					t.Errorf("ParseKittySequence(%q) expected error but got none", tt.input)
				}
				return
			}
			if err != nil {
				t.Errorf("ParseKittySequence(%q) unexpected error: %v", tt.input, err)
				return
			}

			if key.KeyCode != tt.expectedKey {
				t.Errorf("ParseKittySequence(%q) KeyCode = %d, want %d", tt.input, key.KeyCode, tt.expectedKey)
			}
			if key.Modifiers != tt.expectedMod {
				t.Errorf("ParseKittySequence(%q) Modifiers = %d, want %d", tt.input, key.Modifiers, tt.expectedMod)
			}
			if key.EventType != tt.expectedEvent {
				t.Errorf("ParseKittySequence(%q) EventType = %d, want %d", tt.input, key.EventType, tt.expectedEvent)
			}
			if key.Text != tt.expectedText {
				t.Errorf("ParseKittySequence(%q) Text = %q, want %q", tt.input, key.Text, tt.expectedText)
			}
			if width != tt.expectedWidth {
				t.Errorf("ParseKittySequence(%q) width = %d, want %d", tt.input, width, tt.expectedWidth)
			}
		})
	}
}

func TestKittyKeyToLegacyKey(t *testing.T) {
	tests := []struct {
		name          string
		key      Key
		expectedType  KeyType
		expectedAlt   bool
		expectedRunes []rune
	}{
		{
			name: "Simple character",
			key: Key{
				KeyCode:   97, // 'a'
				Modifiers: 0,
				EventType: KittyKeyPress,
			},
			expectedType:  KeyRunes,
			expectedAlt:   false,
			expectedRunes: []rune{'a'},
		},
		{
			name: "Alt+character",
			key: Key{
				KeyCode:   97, // 'a'
				Modifiers: KeyModAlt,
				EventType: KittyKeyPress,
			},
			expectedType:  KeyRunes,
			expectedAlt:   true,
			expectedRunes: []rune{'a'},
		},
		{
			name: "Arrow key",
			key: Key{
				KeyCode:   KittyKeyUp,
				Modifiers: 0,
				EventType: KittyKeyPress,
			},
			expectedType: KeyUp,
			expectedAlt:  false,
		},
		{
			name: "Ctrl+Shift+Arrow",
			key: Key{
				KeyCode:   KittyKeyLeft,
				Modifiers: KeyModCtrl | KeyModShift,
				EventType: KittyKeyPress,
			},
			expectedType: KeyCtrlShiftLeft,
			expectedAlt:  false,
		},
		{
			name: "Function key",
			key: Key{
				KeyCode:   KittyKeyF1,
				Modifiers: 0,
				EventType: KittyKeyPress,
			},
			expectedType: KeyF1,
			expectedAlt:  false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			legacyKey := tt.key.toLegacyKey()

			if legacyKey.Type != tt.expectedType {
				t.Errorf("KittyKey.ToLegacyKey() Type = %v, want %v", legacyKey.Type, tt.expectedType)
			}
			if legacyKey.Alt != tt.expectedAlt {
				t.Errorf("KittyKey.ToLegacyKey() Alt = %v, want %v", legacyKey.Alt, tt.expectedAlt)
			}
			if tt.expectedRunes != nil {
				if len(legacyKey.Runes) != len(tt.expectedRunes) {
					t.Errorf("KittyKey.ToLegacyKey() Runes length = %d, want %d", len(legacyKey.Runes), len(tt.expectedRunes))
				} else {
					for i, r := range tt.expectedRunes {
						if legacyKey.Runes[i] != r {
							t.Errorf("KittyKey.ToLegacyKey() Runes[%d] = %v, want %v", i, legacyKey.Runes[i], r)
						}
					}
				}
			}
		})
	}
}

func TestKittyKeyString(t *testing.T) {
	tests := []struct {
		name     string
		key      Key
		expected string
	}{
		{
			name: "Simple character",
			key: Key{
				KeyCode:   97, // 'a'
				Modifiers: 0,
			},
			expected: "a",
		},
		{
			name: "Ctrl+Alt+character",
			key: Key{
				KeyCode:   97, // 'a'
				Modifiers: KeyModCtrl | KeyModAlt,
			},
			expected: "ctrl+alt+a",
		},
		{
			name: "Super+Shift+F1",
			key: Key{
				KeyCode:   KittyKeyF1,
				Modifiers: KeyModSuper | KeyModShift,
			},
			expected: "super+shift+f1",
		},
		{
			name: "Arrow key",
			key: Key{
				KeyCode: KittyKeyUp,
			},
			expected: "up",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := tt.key.String()
			if result != tt.expected {
				t.Errorf("KittyKey.String() = %q, want %q", result, tt.expected)
			}
		})
	}
}
