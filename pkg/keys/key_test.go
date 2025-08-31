package keys

import (
	"testing"

	"github.com/cfoust/cy/pkg/emu"

	"github.com/stretchr/testify/require"
)

func TestKeysToMsg(t *testing.T) {
	require.Equal(t, []Key{
		{
			Runes: []rune("test"),
			Type:  KeyEventPress,
		},
		{
			Runes: []rune{'a'},
			Mod:   KeyModCtrl,
			Type:  KeyEventPress,
		},
		{
			Runes: []rune{'a'},
			Mod:   KeyModCtrl | KeyModAlt,
			Type:  KeyEventPress,
		},
		{
			Runes: []rune("o"),
			Mod:   KeyModAlt,
			Type:  KeyEventPress,
		},
		{
			Runes: []rune("й"),
			Mod:   KeyModAlt,
			Type:  KeyEventPress,
		},
	}, KeysToMsg(
		"test",
		"ctrl+a",
		"alt+ctrl+a",
		"alt+o",
		"alt+й",
	))
}

func TestKeysToBytes(t *testing.T) {
	keys := []Key{
		{
			Runes: []rune("test"),
			Type:  KeyEventPress,
		},
		{
			Runes: []rune{'a'},
			Type:  KeyEventPress,
		},
		{
			Runes: []rune{KittyKeyEscape},
			Type:  KeyEventPress,
		},
		{
			Runes: []rune{'a'},
			Mod:   KeyModAlt,
			Type:  KeyEventPress,
		},
		{
			Runes: []rune{' '},
			Type:  KeyEventPress,
		},
	}

	for _, key := range keys {
		bytes := key.Bytes(emu.KeyLegacy)

		parsed := make([]Key, 0)
		for i, w := 0, 0; i < len(bytes); i += w {
			var msg Event
			w, msg = Read(bytes[i:])
			if key, ok := msg.(Key); ok {
				parsed = append(parsed, key)
			}
		}
		require.Equal(t, []Key{key}, parsed)
	}
}

func TestDetect(t *testing.T) {
	type testCase struct {
		input []byte
		msg   Event
	}
	cases := []testCase{
		{
			input: []byte("\x1b"),
			msg:   k(KittyKeyEscape),
		},
		{
			input: []byte("\x1bo"),
			msg:   kMod('o', KeyModAlt),
		},
		{
			input: []byte("test"),
			msg:   Key{Runes: []rune("test")},
		},
		{
			input: []byte{byte(keyETX)},
			msg: Key{
				Runes: []rune("c"),
				Mod:   KeyModCtrl,
			},
		},
	}

	for _, c := range cases {
		_, msg := Read(c.input)
		require.Equal(
			t,
			c.msg,
			msg,
			"input '%+v' produced invalid Key",
			c.input,
		)
	}
}
