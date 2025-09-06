package keys

import (
	"testing"

	"github.com/cfoust/cy/pkg/emu"

	"github.com/stretchr/testify/assert"
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

type inCase struct {
	name  string
	input string
	msg   any
}

func in(name string, input string, msg any) inCase {
	return inCase{
		name:  name,
		input: input,
		msg:   msg,
	}
}

func TestDeserialize(t *testing.T) {
	cases := []inCase{
		// legacy
		in("ctrl+c", string([]rune{keyETX}), kMod('c', KeyModCtrl)),
		in("escape", "\x1b", k(KittyKeyEscape)),
		in("alt+o", "\x1bo", kMod('o', KeyModAlt)),
		// kitty
		in("shifted a", "\x1b[97:65;2;65u", Key{
			Code:    'a',
			Mod:     KeyModShift,
			Shifted: 'A',
			Text:    "A",
		}),
		in("esc press", "\x1b[27u", Key{
			Code: KittyKeyEscape,
		}),
		in("home", "\x1b[H", Key{
			Code: KittyKeyHome,
		}),
		in("home release", "\x1b[;1:3H", Key{
			Code: KittyKeyHome,
			Type: KeyEventRelease,
		}),
		in("f5", "\x1b[15~", Key{
			Code: KittyKeyF5,
		}),
		in("f5 release", "\x1b[15;1:3~", Key{
			Code: KittyKeyF5,
			Type: KeyEventRelease,
		}),
		in("left shift", "\x1b[57441;2u", Key{
			Code: KittyLeftShift,
			Mod:  KeyModShift,
		}),
		in("left shift release", "\x1b[57441;1:3u", Key{
			Code: KittyLeftShift,
			Type: KeyEventRelease,
		}),
	}

	for _, test := range cases {
		t.Run(test.name, func(t *testing.T) {
			msg, _ := Read([]byte(test.input))
			assert.Equal(
				t,
				test.msg,
				msg,
				"input '%+v' produced invalid Key",
				[]byte(test.input),
			)
		})
	}
}

type outCase struct {
	name     string
	msg      any
	protocol emu.KeyProtocol
	output   string
}

func out(name string, input string, msg any) outCase {
	return outCase{
		name:   name,
		msg:    msg,
		output: input,
	}
}

func TestKeys(t *testing.T) {
	var cases []outCase

	for _, test := range cases {
		t.Run(test.name, func(t *testing.T) {
			var data []byte
			switch msg := test.msg.(type) {
			case Mouse:
				data = msg.Bytes()
			case Key:
				data = msg.Bytes(test.protocol)
			}

			assert.Equal(
				t,
				[]byte(test.output),
				data,
				"Key produced invalid output",
			)
		})
	}
}
