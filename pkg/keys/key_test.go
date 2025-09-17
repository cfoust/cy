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
		in("alt+o", "\x1bo", Key{
			Code: 'o',
			Mod:  KeyModAlt,
			Text: "o",
		}),
		in("a", "a", Key{
			Code: 'a',
			Text: "a",
		}),
		in("shift+a", "A", Key{
			Code:    'a',
			Shifted: 'A',
			Mod:     KeyModShift,
			Text:    "A",
		}),
		// kitty
		in("ru: л", "\x1b[1083::107u", Key{
			Code: 'л',
			Base: 'k',
		}),
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

		// TODO(cfoust): 09/08/25 Test for multiple codepoints in text
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

type output struct {
	protocol emu.KeyProtocol
	text     string
}

type outCase struct {
	name    string
	key     Key
	outputs []output
}

func out(
	name string,
	key Key,
	cases ...any,
) (o outCase) {
	o.name = name
	o.key = key

	for i := 0; i < len(cases); i += 2 {
		o.outputs = append(o.outputs, output{
			protocol: cases[i].(emu.KeyProtocol),
			text:     cases[i+1].(string),
		})
	}

	return
}

type legacyOutput struct {
	mod  KeyModifiers
	text string
}

type legacyCase struct {
	name    string
	r       rune
	outputs []legacyOutput
}

func legacyOut(
	name string,
	r rune,
	cases ...any,
) (o legacyCase) {
	o.name = name
	o.r = r

	for i := 0; i < len(cases); i += 2 {
		o.outputs = append(o.outputs, legacyOutput{
			mod:  cases[i].(KeyModifiers),
			text: cases[i+1].(string),
		})
	}

	return
}

func TestKeys(t *testing.T) {
	const (
		legacy       = emu.KeyLegacy
		disambiguate = emu.KeyDisambiguate
		types        = emu.KeyReportEventTypes
		alt          = emu.KeyReportAlternateKeys
		text         = emu.KeyReportAssociatedText
		all          = emu.KeyReportAllKeys
	)

	cases := []outCase{
		out(
			"rune",
			Key{
				Code:    'o',
				Shifted: 'O',
				Text:    "o",
			},
			legacy, "o",
			// do nothing without all
			types, "o",
			alt, "o",
			disambiguate, "o", // not a legacy key combo
			all, "\x1b[111u",
			all|alt, "\x1b[111:79u",
			all|text, "\x1b[111;;111u",
			all|types, "\x1b[111u",
			all|text|types, "\x1b[111;;111u",
		),
		out(
			"legacy text key",
			Key{
				Code:    'c',
				Shifted: 'C',
				Mod:     KeyModCtrl,
				Text:    "c",
			},
			legacy, string(keyETX), // ctrl+c
			disambiguate, "\x1b[99;5u",
			all, "\x1b[99;5u",
			all|text|types, "\x1b[99;5;99u",
		),
		out(
			"shift+rune",
			Key{
				Code:    'o',
				Shifted: 'O',
				Text:    "O",
				Mod:     KeyModShift,
			},
			legacy, "O",
		),
	}

	for _, c := range []legacyCase{
		legacyOut(
			"tab",
			KittyKeyTab,
			KeyModifiers(0), "\x09",
			KeyModCtrl, "\x09",
			KeyModAlt, "\x1b\x09",
			KeyModShift, "\x1b[Z",
			KeyModAlt|KeyModShift, "\x1b\x1b[Z",
			KeyModAlt|KeyModCtrl, "\x1b\x09",
		),
		legacyOut(
			"modifier key alone",
			KittyLeftControl,
			KeyModifiers(0), "",
		),
	} {
		for _, o := range c.outputs {
			cases = append(cases, outCase{
				name: c.name,
				key: Key{
					Code: c.r,
					Mod:  o.mod,
				},
				outputs: []output{
					{
						protocol: legacy,
						text:     o.text,
					},
				},
			})
		}
	}

	for _, test := range cases {
		t.Run(test.name, func(t *testing.T) {
			for _, o := range test.outputs {
				t.Run(o.protocol.String(), func(t *testing.T) {
					var (
						isNothing = len(o.text) == 0
						data, ok  = test.key.Bytes(
							o.protocol,
						)
					)
					assert.True(
						t,
						ok || isNothing,
					)

					if isNothing {
						assert.Len(t, data, 0)
						return
					}

					assert.Equal(
						t,
						[]byte(o.text),
						data,
						"Key produced invalid output",
					)
				})
			}
		})
	}
}
