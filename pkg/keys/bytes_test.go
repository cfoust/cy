package keys

import (
	"bytes"
	"testing"

	"github.com/cfoust/cy/pkg/emu"
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
				Runes: []rune{'a'},
				Type:  KeyEventPress,
			},
			expected: []byte("a"),
		},
		{
			name: "Alt+character",
			key: Key{
				Runes:     []rune{'a'},
				Mod: KeyModAlt,
				Type:      KeyEventPress,
			},
			expected: []byte("\x1ba"),
		},
		{
			name: "Space key",
			key: Key{
				Runes: []rune{' '},
				Type:  KeyEventPress,
			},
			expected: []byte(" "),
		},
		{
			name: "Enter key",
			key: Key{
				Runes: []rune{KittyKeyEnter},
				Type:  KeyEventPress,
			},
			expected: []byte("\r"),
		},
		{
			name: "Escape key",
			key: Key{
				Runes: []rune{KittyKeyEscape},
				Type:  KeyEventPress,
			},
			expected: []byte("\x1b"),
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := tt.key.Bytes(emu.KeyLegacy)
			if !bytes.Equal(result, tt.expected) {
				t.Errorf("Key.Bytes() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestKittyKeyBytes(t *testing.T) {
	tests := []struct {
		name     string
		key      Key
		expected []byte
	}{
		{
			name: "Simple character - Kitty protocol",
			key: Key{
				Runes: []rune{97}, // 'a'
				Type:  KeyEventPress,
			},
			expected: []byte("\x1b[97u"),
		},
		{
			name: "Character with Alt modifier - Kitty protocol",
			key: Key{
				Runes:     []rune{97}, // 'a'
				Mod: KeyModAlt,
				Type:      KeyEventPress,
			},
			expected: []byte("\x1b[97;2u"),
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := tt.key.Bytes(emu.KeyReportEventTypes)
			if !bytes.Equal(result, tt.expected) {
				t.Errorf("Key.Bytes(Kitty) = %v (%s), want %v (%s)",
					result, string(result), tt.expected, string(tt.expected))
			}
		})
	}
}
