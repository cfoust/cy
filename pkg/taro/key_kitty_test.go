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
		expectedMod   KittyModifiers
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
			expectedMod:   KittyModifiers(2), // Shift
			expectedEvent: KittyKeyPress,
			expectedWidth: 7,
			shouldError:   false,
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
			if width != tt.expectedWidth {
				t.Errorf("ParseKittySequence(%q) width = %d, want %d", tt.input, width, tt.expectedWidth)
			}
		})
	}
}

func TestKittyKeyToLegacyKey(t *testing.T) {
	tests := []struct {
		name          string
		kittyKey      KittyKey
		expectedType  KeyType
		expectedAlt   bool
		expectedRunes []rune
	}{
		{
			name: "Simple character",
			kittyKey: KittyKey{
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
			kittyKey: KittyKey{
				KeyCode:   97, // 'a'
				Modifiers: KittyModAlt,
				EventType: KittyKeyPress,
			},
			expectedType:  KeyRunes,
			expectedAlt:   true,
			expectedRunes: []rune{'a'},
		},
		{
			name: "Arrow key",
			kittyKey: KittyKey{
				KeyCode:   KittyKeyUp,
				Modifiers: 0,
				EventType: KittyKeyPress,
			},
			expectedType: KeyUp,
			expectedAlt:  false,
		},
		{
			name: "Ctrl+Shift+Arrow",
			kittyKey: KittyKey{
				KeyCode:   KittyKeyLeft,
				Modifiers: KittyModCtrl | KittyModShift,
				EventType: KittyKeyPress,
			},
			expectedType: KeyCtrlShiftLeft,
			expectedAlt:  false,
		},
		{
			name: "Function key",
			kittyKey: KittyKey{
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
			legacyKey := tt.kittyKey.ToLegacyKey()

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
		kittyKey KittyKey
		expected string
	}{
		{
			name: "Simple character",
			kittyKey: KittyKey{
				KeyCode:   97, // 'a'
				Modifiers: 0,
			},
			expected: "a",
		},
		{
			name: "Ctrl+Alt+character",
			kittyKey: KittyKey{
				KeyCode:   97, // 'a'
				Modifiers: KittyModCtrl | KittyModAlt,
			},
			expected: "ctrl+alt+a",
		},
		{
			name: "Super+Shift+F1",
			kittyKey: KittyKey{
				KeyCode:   KittyKeyF1,
				Modifiers: KittyModSuper | KittyModShift,
			},
			expected: "super+shift+f1",
		},
		{
			name: "Arrow key",
			kittyKey: KittyKey{
				KeyCode: KittyKeyUp,
			},
			expected: "up",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := tt.kittyKey.String()
			if result != tt.expected {
				t.Errorf("KittyKey.String() = %q, want %q", result, tt.expected)
			}
		})
	}
}

func TestNewKittyKey(t *testing.T) {
	kittyKey := KittyKey{
		KeyCode:   97, // 'a'
		Modifiers: KittyModAlt,
		EventType: KittyKeyPress,
	}

	key := NewKittyKey(kittyKey)

	// Should have legacy fields filled correctly
	if key.Type != KeyRunes {
		t.Errorf("NewKittyKey() Type = %v, want %v", key.Type, KeyRunes)
	}
	if !key.Alt {
		t.Errorf("NewKittyKey() Alt = %v, want %v", key.Alt, true)
	}
	if len(key.Runes) != 1 || key.Runes[0] != 'a' {
		t.Errorf("NewKittyKey() Runes = %v, want %v", key.Runes, []rune{'a'})
	}

	// Should have Kitty information preserved
	if key.Kitty == nil {
		t.Errorf("NewKittyKey() Kitty is nil, want non-nil")
	} else {
		if key.Kitty.KeyCode != 97 {
			t.Errorf("NewKittyKey() Kitty.KeyCode = %d, want %d", key.Kitty.KeyCode, 97)
		}
		if key.Kitty.Modifiers != KittyModAlt {
			t.Errorf("NewKittyKey() Kitty.Modifiers = %v, want %v", key.Kitty.Modifiers, KittyModAlt)
		}
	}

	// Test enhanced methods
	if !key.HasKittyProtocol() {
		t.Errorf("NewKittyKey() HasKittyProtocol() = false, want true")
	}
	if !key.IsPress() {
		t.Errorf("NewKittyKey() IsPress() = false, want true")
	}
	if !key.HasAlt() {
		t.Errorf("NewKittyKey() HasAlt() = false, want true")
	}
	if key.HasCtrl() {
		t.Errorf("NewKittyKey() HasCtrl() = true, want false")
	}
}
