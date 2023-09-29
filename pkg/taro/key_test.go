package taro

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestKeysToMsg(t *testing.T) {
	assert.Equal(t, []KeyMsg{
		{
			Type:  KeyRunes,
			Runes: []rune("test"),
		},
		{
			Type: KeyCtrlA,
		},
	}, KeysToMsg("test", "ctrl+a"))
}

func TestKeysToBytes(t *testing.T) {
	keys := []KeyMsg{
		{
			Type:  KeyRunes,
			Runes: []rune("test"),
		},
		{
			Type: KeyCtrlA,
		},
		{
			Type: keyETX,
		},
	}

	bytes, err := KeysToBytes(keys...)
	assert.NoError(t, err)

	parsed := make([]KeyMsg, 0)
	for i, w := 0, 0; i < len(bytes); i += w {
		var msg Msg
		w, msg = DetectOneMsg(bytes[i:])
		if key, ok := msg.(KeyMsg); ok {
			parsed = append(parsed, key)
		}
	}
	assert.Equal(t, keys, parsed)
}
