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
		{
			Type:  KeyRunes,
			Runes: []rune("o"),
			Alt:   true,
		},
		{
			Type:  KeyRunes,
			Runes: []rune("й"),
			Alt:   true,
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
			Type:  KeyRunes,
			Runes: []rune("test"),
		},
		{
			Type: KeyCtrlA,
		},
		{
			Type: keyETX,
		},
		{
			Type: keyESC,
		},
		{
			Type: keyESC,
			Alt:  true,
		},
		{
			Type:  KeyRunes,
			Runes: []rune("a"),
			Alt:   true,
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
				Type: KeyEscape,
			},
		},
		{
			input: []byte("\x1bo"),
			msg: KeyMsg{
				Type:  KeyRunes,
				Runes: []rune("o"),
				Alt:   true,
			},
		},
	}

	for _, c := range cases {
		_, msg := DetectOneMsg(c.input)
		assert.Equal(t, c.msg, msg)
	}
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
