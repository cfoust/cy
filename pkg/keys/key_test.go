package keys

import (
	"testing"

	"github.com/cfoust/cy/pkg/emu"

	"github.com/stretchr/testify/require"
)

func TestFromNames(t *testing.T) {
	require.Equal(t, []Key{
		{
			Code: KeyText,
			Text: "test",
		},
		kMod('a', KeyModCtrl),
		kMod('a', KeyModCtrl|KeyModAlt),
		kMod('o', KeyModAlt),
		// TODO(cfoust): 09/05/25 return to this
		//kMod('й', KeyModAlt),
	}, FromNames(
		"test",
		"ctrl+a",
		"alt+ctrl+a",
		"alt+o",
		//"alt+й",
	))
}

func TestKeysToBytes(t *testing.T) {
	t.Skip()
	keys := []Key{
		{
			Code: KeyText,
			Text: "test",
		},
		{
			Code: 'a',
		},
		{
			Code: KittyKeyEscape,
		},
		{
			Code: 'a',
			Mod:  KeyModAlt,
		},
		{
			Code: ' ',
		},
	}

	for _, key := range keys {
		bytes := key.Bytes(emu.KeyLegacy)

		parsed := make([]Key, 0)
		for i, w := 0, 0; i < len(bytes); i += w {
			var msg any
			msg, w = Read(bytes[i:])
			if key, ok := msg.(Key); ok {
				parsed = append(parsed, key)
			}
		}
		require.Equal(t, []Key{key}, parsed)
	}
}

type testCase struct {
	input    string
	msg      any
	protocol emu.KeyProtocol
	output   string
}

func same(input string, msg any) testCase {
	return testCase{
		input:  input,
		msg:    msg,
		output: input,
	}
}

func TestKeys(t *testing.T) {
	cases := []testCase{
		same("\x1b", k(KittyKeyEscape)),
		same("\x1bo", kMod('o', KeyModAlt)),
		same("test", Key{
			Code: KeyText,
			Text: "test",
		}),
		same(string([]rune{keyETX}), kMod('c', KeyModCtrl)),
	}
	for _, test := range cases {
		msg, _ := Read([]byte(test.input))
		require.Equal(
			t,
			test.msg,
			msg,
			"input '%+v' produced invalid Key",
			test.input,
		)

		var data []byte
		switch msg := msg.(type) {
		case MouseEvent:
			data = msg.Bytes()
		case Key:
			data = msg.Bytes(test.protocol)
		}

		require.Equal(
			t,
			[]byte(test.output),
			data,
			"input '%+v' produced invalid output",
			test.input,
		)
	}
}
