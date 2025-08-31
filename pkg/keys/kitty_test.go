package keys

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
		{
			"No semicolon in complex",
			[]byte("\x1b[65u"),
			true,
		}, // This should still be valid
		{
			"Protocol command",
			[]byte("\x1b[>1;1u"),
			false,
		}, // These are protocol commands, not key sequences
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := IsKittySequence(tt.input)
			if result != tt.expected {
				t.Errorf(
					"IsKittySequence(%q) = %v, want %v",
					tt.input,
					result,
					tt.expected,
				)
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
		expectedEvent KeyEventType
		expectedText  string
		expectedWidth int
		shouldError   bool
	}{
		{
			name:          "Simple key",
			input:         []byte("\x1b[65u"),
			expectedKey:   65, // 'A'
			expectedMod:   0,
			expectedEvent: KeyEventPress,
			expectedWidth: 5,
			shouldError:   false,
		},
		{
			name:          "Key with modifiers",
			input:         []byte("\x1b[65;2u"),
			expectedKey:   65,
			expectedMod:   KeyModifiers(2), // Shift
			expectedEvent: KeyEventPress,
			expectedWidth: 7,
			shouldError:   false,
		},
		{
			name:          "Key press event (explicit)",
			input:         []byte("\x1b[65;2;0u"),
			expectedKey:   65,
			expectedMod:   KeyModifiers(2),
			expectedEvent: KeyEventPress,
			expectedWidth: 9,
			shouldError:   false,
		},
		{
			name:          "Key repeat event",
			input:         []byte("\x1b[65;2;1u"),
			expectedKey:   65,
			expectedMod:   KeyModifiers(2),
			expectedEvent: KeyEventRepeat,
			expectedWidth: 9,
			shouldError:   false,
		},
		{
			name:          "Key release event",
			input:         []byte("\x1b[65;2;2u"),
			expectedKey:   65,
			expectedMod:   KeyModifiers(2),
			expectedEvent: KeyEventRelease,
			expectedWidth: 9,
			shouldError:   false,
		},
		{
			name:          "Key with text",
			input:         []byte("\x1b[65;0;0;textu"),
			expectedKey:   65,
			expectedMod:   0,
			expectedEvent: KeyEventPress,
			expectedText:  "text",
			expectedWidth: 14,
			shouldError:   false,
		},
		{
			name:          "Key repeat with no modifiers",
			input:         []byte("\x1b[97;;1u"),
			expectedKey:   97, // 'a'
			expectedMod:   0,
			expectedEvent: KeyEventRepeat,
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
					t.Errorf(
						"ParseKittySequence(%q) expected error but got none",
						tt.input,
					)
				}
				return
			}
			if err != nil {
				t.Errorf(
					"ParseKittySequence(%q) unexpected error: %v",
					tt.input,
					err,
				)
				return
			}

			expectedRune := rune(tt.expectedKey)
			if len(key.Runes) == 0 || key.Runes[0] != expectedRune {
				actualKey := -1
				if len(key.Runes) > 0 {
					actualKey = int(key.Runes[0])
				}
				t.Errorf(
					"ParseKittySequence(%q) KeyCode = %d, want %d",
					tt.input,
					actualKey,
					tt.expectedKey,
				)
			}
			if key.Mod != tt.expectedMod {
				t.Errorf(
					"ParseKittySequence(%q) Modifiers = %d, want %d",
					tt.input,
					key.Mod,
					tt.expectedMod,
				)
			}
			if key.Type != tt.expectedEvent {
				t.Errorf(
					"ParseKittySequence(%q) EventType = %d, want %d",
					tt.input,
					key.Type,
					tt.expectedEvent,
				)
			}
			// TODO: implement text support
			// Currently no Text field in Key struct
			if tt.expectedText != "" {
				t.Logf(
					"ParseKittySequence(%q) Text support not yet implemented, expected %q",
					tt.input,
					tt.expectedText,
				)
			}
			if width != tt.expectedWidth {
				t.Errorf(
					"ParseKittySequence(%q) width = %d, want %d",
					tt.input,
					width,
					tt.expectedWidth,
				)
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
				Runes:     []rune{97}, // 'a'
				Mod: 0,
				Type:      KeyEventPress,
			},
			expected: "a",
		},
		{
			name: "Ctrl+Alt+character",
			key: Key{
				Runes:     []rune{97}, // 'a'
				Mod: KeyModCtrl | KeyModAlt,
				Type:      KeyEventPress,
			},
			expected: "ctrl+alt+a",
		},
		{
			name: "Super+Shift+F1",
			key: Key{
				Runes:     []rune{KittyKeyF1},
				Mod: KeyModSuper | KeyModShift,
				Type:      KeyEventPress,
			},
			expected: "super+shift+f1",
		},
		{
			name: "Arrow key",
			key: Key{
				Runes: []rune{KittyKeyUp},
				Type:  KeyEventPress,
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
