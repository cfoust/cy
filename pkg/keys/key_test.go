package keys

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestKeysToMsg(t *testing.T) {
	assert.Equal(t, []Key{
		{
			Runes: []rune("test"),
			Type:  KeyEventPress,
		},
		{
			Runes:     []rune{'a'},
			Mod: KeyModCtrl,
			Type:      KeyEventPress,
		},
		{
			Runes:     []rune{'a'},
			Mod: KeyModCtrl | KeyModAlt,
			Type:      KeyEventPress,
		},
		{
			Runes:     []rune("o"),
			Mod: KeyModAlt,
			Type:      KeyEventPress,
		},
		{
			Runes:     []rune("й"),
			Mod: KeyModAlt,
			Type:      KeyEventPress,
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
	keys := []KeyMsg{
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
			Runes:     []rune{'a'},
			Modifiers: KeyModAlt,
			Type:      KeyEventPress,
		},
		{
			Runes: []rune{' '},
			Type:  KeyEventPress,
		},
	}

	for _, key := range keys {
		bytes, err := KeysToBytes(key)
		assert.NoError(t, err)

		parsed := make([]KeyMsg, 0)
		for i, w := 0, 0; i < len(bytes); i += w {
			var msg Msg
			w, msg = DetectOneMsg(bytes[i:])
			if key, ok := msg.(KeyMsg); ok {
				parsed = append(parsed, key)
			}
		}
		assert.Equal(t, []KeyMsg{key}, parsed)
	}
}

func TestDetect(t *testing.T) {
	type testCase struct {
		input []byte
		msg   Msg
	}
	cases := []testCase{
		{
			input: []byte("\x1b"),
			msg: KeyMsg{
				Runes: []rune{KittyKeyEscape},
				Type:  KeyEventPress,
			},
		},
		{
			input: []byte("\x1bo"),
			msg: KeyMsg{
				Runes:     []rune("o"),
				Modifiers: KeyModAlt,
			},
		},
		{
			input: []byte("test"),
			msg: KeyMsg{
				Runes: []rune("test"),
			},
		},
		{
			input: []byte{byte(KeyCtrlC)},
			msg: KeyMsg{
				Runes:     []rune("c"),
				Modifiers: KeyModCtrl,
			},
		},
	}

	for _, c := range cases {
		_, msg := DetectOneMsg(c.input)
		assert.Equal(
			t,
			c.msg,
			msg,
			"input '%+v' produced invalid Key",
			c.input,
		)
	}
}
