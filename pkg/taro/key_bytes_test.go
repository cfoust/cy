package taro

import (
	"bytes"
	"testing"
)

func TestKeyBytes(t *testing.T) {
	tests := []struct {
		name     string
		key      Key
		expected []byte
	}{
		{
			name: "Simple character",
			key: Key{
				Type:  KeyRunes,
				Runes: []rune{'a'},
			},
			expected: []byte("a"),
		},
		{
			name: "Alt+character",
			key: Key{
				Type:  KeyRunes,
				Runes: []rune{'a'},
				Alt:   true,
			},
			expected: []byte("\x1ba"),
		},
		{
			name: "Space key",
			key: Key{
				Type: KeySpace,
			},
			expected: []byte(" "),
		},
		{
			name: "Enter key",
			key: Key{
				Type: KeyEnter,
			},
			expected: []byte("\r"),
		},
		{
			name: "Escape key",
			key: Key{
				Type: KeyEscape,
			},
			expected: []byte("\x1b"),
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := tt.key.Bytes()
			if !bytes.Equal(result, tt.expected) {
				t.Errorf("Key.Bytes() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestKittyKeyBytes(t *testing.T) {
	tests := []struct {
		name     string
		key      KittyKey
		expected []byte
	}{
		{
			name: "Simple character",
			key: KittyKey{
				KeyCode:   97, // 'a'
				Modifiers: 0,
				EventType: KittyKeyPress,
			},
			expected: []byte("\x1b[97u"),
		},
		{
			name: "Character with Alt modifier",
			key: KittyKey{
				KeyCode:   97, // 'a'
				Modifiers: KittyModAlt,
				EventType: KittyKeyPress,
			},
			expected: []byte("\x1b[97;2u"),
		},
		{
			name: "Character with multiple modifiers",
			key: KittyKey{
				KeyCode:   97, // 'a'
				Modifiers: KittyModCtrl | KittyModShift,
				EventType: KittyKeyPress,
			},
			expected: []byte("\x1b[97;5u"), // 4 (Ctrl) + 1 (Shift) = 5
		},
		{
			name: "Arrow key",
			key: KittyKey{
				KeyCode:   KittyKeyUp,
				Modifiers: 0,
				EventType: KittyKeyPress,
			},
			expected: []byte("\x1b[57409u"), // KittyKeyUp = 0xE000 + 65 = 57409
		},
		{
			name: "Key with event type",
			key: KittyKey{
				KeyCode:   97, // 'a'
				Modifiers: 0,
				EventType: KittyKeyRepeat,
			},
			expected: []byte("\x1b[97;0;1u"), // Event type 1 = repeat
		},
		{
			name: "Key with text",
			key: KittyKey{
				KeyCode:   97, // 'a'
				Modifiers: 0,
				EventType: KittyKeyPress,
				Text:      "test",
			},
			expected: []byte("\x1b[97;0;0;testu"), // With text
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := tt.key.Bytes()
			if !bytes.Equal(result, tt.expected) {
				t.Errorf("KittyKey.Bytes() = %v (%s), want %v (%s)",
					result, string(result), tt.expected, string(tt.expected))
			}
		})
	}
}

func TestNewKittyKeyBytes(t *testing.T) {
	// Test that a Key created with NewKittyKey still produces correct bytes
	kittyKey := KittyKey{
		KeyCode:   97, // 'a'
		Modifiers: KittyModAlt,
		EventType: KittyKeyPress,
	}

	key := NewKittyKey(kittyKey)

	// The key should have legacy representation
	legacyBytes := key.Bytes()
	expectedLegacy := []byte("\x1ba") // Alt+a in legacy format

	if !bytes.Equal(legacyBytes, expectedLegacy) {
		t.Errorf("NewKittyKey().Bytes() = %v (%s), want %v (%s)",
			legacyBytes, string(legacyBytes), expectedLegacy, string(expectedLegacy))
	}

	// The KittyKey should produce Kitty protocol bytes
	kittyBytes := kittyKey.Bytes()
	expectedKitty := []byte("\x1b[97;2u") // Alt+a in Kitty format

	if !bytes.Equal(kittyBytes, expectedKitty) {
		t.Errorf("KittyKey.Bytes() = %v (%s), want %v (%s)",
			kittyBytes, string(kittyBytes), expectedKitty, string(expectedKitty))
	}
}
