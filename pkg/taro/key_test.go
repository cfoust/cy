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
		{
			Type: KeyCtrlA,
			Alt:  true,
		},
	}, KeysToMsg("test", "ctrl+a", "alt+ctrl+a"))
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

func testMouseInput(t *testing.T, input string) {
	bytes := []byte(input)
	_, msg := DetectOneMsg(bytes)
	mouse, ok := msg.(MouseMsg)
	if !ok {
		t.Fail()
	}
	assert.Equal(t, bytes, mouse.Bytes())
}

func TestMouse(t *testing.T) {
	testMouseInput(t, "\u001b[M :;")
	testMouseInput(t, "\u001b[M@;;")
	testMouseInput(t, "\u001b[M`9>")
	testMouseInput(t, "\u001b[MaD3")
	testMouseInput(t, "\u001b[Mc>;")
	testMouseInput(t, "\u001b[Mc74")
	testMouseInput(t, "\u001b[MC}2")
	testMouseInput(t, "\u001b[M@P6")
	testMouseInput(t, "\u001b[M#Q6")
	testMouseInput(t, "\u001b[MCu,")
	testMouseInput(t, "\u001b[MbM<")
}
