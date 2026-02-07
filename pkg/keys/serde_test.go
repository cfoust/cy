package keys

import (
	"testing"

	"github.com/cfoust/cy/pkg/emu"

	"github.com/stretchr/testify/assert"
)

type deCase struct {
	name  string
	input string
	msg   any
}

func de(name string, input string, msg any) deCase {
	return deCase{
		name:  name,
		input: input,
		msg:   msg,
	}
}

func TestDeserialize(t *testing.T) {
	cases := []deCase{
		// legacy
		de("ctrl+c", string([]rune{keyETX}), kMod('c', KeyModCtrl)),
		de("ctrl+[", "\x1b", kMod('[', KeyModCtrl)),
		de("ctrl+m", "\x0d", kMod('m', KeyModCtrl)),
		de("ctrl+i", "\x09", kMod('i', KeyModCtrl)),
		de("ctrl+?", "\x7f", kMod('?', KeyModCtrl)),
		de("alt+o", "\x1bo", Key{
			Code:    'o',
			Shifted: 'O',
			Mod:     KeyModAlt,
			Text:    "o",
		}),
		de("a", "a", Key{
			Code:    'a',
			Shifted: 'A',
			Text:    "a",
		}),
		de("shift+a", "A", Key{
			Code:    'a',
			Shifted: 'A',
			Mod:     KeyModShift,
			Text:    "A",
		}),
		de("zh", "致", Key{
			Code: '致',
			Text: "致",
		}),
		de("ru", "ж", Key{
			Code:    'ж',
			Shifted: 'Ж',
			Text:    "ж",
		}),
		de("ru shifted", "Ж", Key{
			Code:    'ж',
			Shifted: 'Ж',
			Mod:     KeyModShift,
			Text:    "Ж",
		}),
		de("shift+semicolon", ":", Key{
			Code:    ';',
			Shifted: ':',
			Mod:     KeyModShift,
			Text:    ":",
		}),
		de("text", "test", Key{
			Code: KeyText,
			Text: "test",
		}),
		de("text with spaces", "foo bar baz", Key{
			Code: KeyText,
			Text: "foo bar baz",
		}),
		de("text with return", "foo\nbar", Key{
			Code: KeyText,
			Text: "foo\nbar",
		}),
		// bracketed paste
		de("bracketed paste", "\x1b[200~hello world\x1b[201~", Key{
			Code: KeyText,
			Text: "hello world",
		}),
		de(
			"bracketed paste with newlines",
			"\x1b[200~line1\nline2\nline3\x1b[201~",
			Key{
				Code: KeyText,
				Text: "line1\nline2\nline3",
			},
		),
		de("bracketed paste empty", "\x1b[200~\x1b[201~", Key{
			Code: KeyText,
			Text: "",
		}),
		// kitty
		de("ru: л", "\x1b[1083::107u", Key{
			Code: 'л',
			Base: 'k',
		}),
		de("shifted a", "\x1b[97:65;2;65u", Key{
			Code:    'a',
			Mod:     KeyModShift,
			Shifted: 'A',
			Text:    "A",
		}),
		de("esc press", "\x1b[27u", Key{
			Code: KittyKeyEscape,
		}),
		de("home", "\x1b[H", Key{
			Code: KittyKeyHome,
		}),
		de("home release", "\x1b[;1:3H", Key{
			Code: KittyKeyHome,
			Type: KeyEventRelease,
		}),
		de("f5", "\x1b[15~", Key{
			Code: KittyKeyF5,
		}),
		de("f5 release", "\x1b[15;1:3~", Key{
			Code: KittyKeyF5,
			Type: KeyEventRelease,
		}),
		de("left shift", "\x1b[57441;2u", Key{
			Code: KittyLeftShift,
			Mod:  KeyModShift,
		}),
		de("left shift release", "\x1b[57441;1:3u", Key{
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

type output struct {
	protocol emu.KeyProtocol
	text     string
}

type seCase struct {
	name    string
	key     Key
	outputs []output
}

func se(
	name string,
	key Key,
	cases ...any,
) (o seCase) {
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

func TestSerialize(t *testing.T) {
	const (
		legacy       = emu.KeyLegacy
		disambiguate = emu.KeyDisambiguateEscape
		types        = emu.KeyReportEventTypes
		alt          = emu.KeyReportAlternateKeys
		text         = emu.KeyReportAssociatedText
		all          = emu.KeyReportAllKeys
	)

	cases := []seCase{
		se(
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
		se(
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
		se(
			"ctrl+[ (escape)",
			kMod('[', KeyModCtrl),
			legacy, string(keyESC),
			disambiguate, "\x1b[91;5u",
		),
		se(
			"ctrl+m (enter)",
			kMod('m', KeyModCtrl),
			legacy, string(keyCR),
		),
		se(
			"ctrl+i (tab)",
			kMod('i', KeyModCtrl),
			legacy, string(keyHT),
		),
		se(
			"ctrl+? (backspace)",
			kMod('?', KeyModCtrl),
			legacy, string(keyDEL),
		),
		se(
			"shift+space",
			Key{
				Code: ' ',
				Mod:  KeyModShift,
			},
			legacy, " ",
		),
		// Enter, Tab, Backspace WITH modifiers should use CSI encoding
		// because they don't generate text (no Text field)
		se(
			"shift+enter",
			Key{
				Code: KittyKeyEnter,
				Mod:  KeyModShift,
			},
			legacy, string(keyCR),
			disambiguate, "\x1b[13;2u",
		),
		se(
			"ctrl+enter",
			Key{
				Code: KittyKeyEnter,
				Mod:  KeyModCtrl,
			},
			legacy, string(keyCR),
			disambiguate, "\x1b[13;5u",
		),
		se(
			"alt+enter",
			Key{
				Code: KittyKeyEnter,
				Mod:  KeyModAlt,
			},
			legacy, "\x1b\r",
			disambiguate, "\x1b[13;3u",
		),
		se(
			"ctrl+shift+enter",
			Key{
				Code: KittyKeyEnter,
				Mod:  KeyModCtrl | KeyModShift,
			},
			legacy, string(keyCR),
			disambiguate, "\x1b[13;6u",
		),
		se(
			"shift+tab",
			Key{
				Code: KittyKeyTab,
				Mod:  KeyModShift,
			},
			legacy, "\x1b[Z",
			disambiguate, "\x1b[9;2u",
		),
		se(
			"ctrl+backspace",
			Key{
				Code: KittyKeyBackspace,
				Mod:  KeyModCtrl,
			},
			legacy, "\x7f",
			disambiguate, "\x1b[127;5u",
		),
		se(
			"shift+escape",
			Key{
				Code: KittyKeyEscape,
				Mod:  KeyModShift,
			},
			legacy, "\x1b",
			disambiguate, "\x1b[27;2u",
		),
		se(
			"enter no modifier",
			Key{
				Code: KittyKeyEnter,
			},
			legacy, string(keyCR),
			disambiguate, string(keyCR), // stays legacy per spec
		),
		se(
			"tab no modifier",
			Key{
				Code: KittyKeyTab,
			},
			legacy, "\x09",
			disambiguate, "\x09", // stays legacy per spec
		),
		se(
			"backspace no modifier",
			Key{
				Code: KittyKeyBackspace,
			},
			legacy, "\x7f",
			disambiguate, "\x7f", // stays legacy per spec
		),
		// Escape without modifiers IS disambiguated (it's in legacyTextKeys)
		se(
			"escape no modifier",
			Key{
				Code: KittyKeyEscape,
			},
			legacy, "\x1b",
			disambiguate, "\x1b[27u",
		),
		se(
			"shift+rune",
			Key{
				Code:    'o',
				Shifted: 'O',
				Text:    "O",
				Mod:     KeyModShift,
			},
			legacy, "O",
			disambiguate, "O",
		),
		se(
			"shift+colon",
			Key{
				Code:    ';',
				Shifted: ':',
				Text:    ":",
				Mod:     KeyModShift,
			},
			legacy, ":",
			disambiguate, ":",
		),
		se(
			"rune release no modifier",
			Key{
				Code:    'o',
				Shifted: 'O',
				Text:    "o",
				Type:    KeyEventRelease,
			},
			all|types, "\x1b[111;1:3u",
			all|types|text, "\x1b[111;1:3;111u",
		),
		se(
			"rune repeat no modifier",
			Key{
				Code:    'o',
				Shifted: 'O',
				Text:    "o",
				Type:    KeyEventRepeat,
			},
			all|types, "\x1b[111;1:2u",
			all|types|text, "\x1b[111;1:2;111u",
		),
		se(
			"text",
			Key{
				Code: KeyText,
				Text: "test",
			},
			legacy, "test",
			all, "test",
		),
	}

	// Modifier-only keys should only be reported with KeyReportAllKeys (flag 8)
	// Per the Kitty keyboard protocol: "Additionally, with this mode, events
	// for pressing modifier keys are reported"
	modifierOnlyTests := []seCase{
		se(
			"left shift press",
			Key{
				Code: KittyLeftShift,
				Mod:  KeyModShift,
			},
			legacy, "", // not reported
			disambiguate, "", // not reported
			types, "", // not reported
			alt, "", // not reported
			all, "\x1b[57441;2u", // reported only with flag 8
			all|types, "\x1b[57441;2u",
		),
		se(
			"left control press",
			Key{
				Code: KittyLeftControl,
				Mod:  KeyModCtrl,
			},
			legacy, "",
			disambiguate, "",
			all, "\x1b[57442;5u",
		),
		se(
			"left alt press",
			Key{
				Code: KittyLeftAlt,
				Mod:  KeyModAlt,
			},
			legacy, "",
			disambiguate, "",
			all, "\x1b[57443;3u",
		),
		se(
			"right shift press",
			Key{
				Code: KittyRightShift,
				Mod:  KeyModShift,
			},
			legacy, "",
			disambiguate, "",
			all, "\x1b[57447;2u",
		),
		se(
			"modifier release with types",
			Key{
				Code: KittyLeftShift,
				Type: KeyEventRelease,
			},
			all|types, "\x1b[57441;1:3u",
		),
	}
	cases = append(cases, modifierOnlyTests...)

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
			cases = append(cases, seCase{
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
							emu.DefaultMode,
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

func TestBracketedPasteSerialization(t *testing.T) {
	textKey := Key{
		Code: KeyText,
		Text: "hello world",
	}

	// Without bracketed paste mode, text should be returned as-is
	t.Run("without bracketed paste mode", func(t *testing.T) {
		data, ok := textKey.Bytes(emu.DefaultMode, emu.KeyLegacy)
		assert.True(t, ok)
		assert.Equal(t, []byte("hello world"), data)
	})

	// With bracketed paste mode enabled, text should be wrapped
	t.Run("with bracketed paste mode", func(t *testing.T) {
		data, ok := textKey.Bytes(emu.ModeBracketedPaste, emu.KeyLegacy)
		assert.True(t, ok)
		assert.Equal(
			t,
			[]byte("\x1b[200~hello world\x1b[201~"),
			data,
		)
	})

	// Non-text keys should not be affected by bracketed paste mode
	t.Run("non-text key with bracketed paste mode", func(t *testing.T) {
		key := Key{
			Code:    'a',
			Shifted: 'A',
			Text:    "a",
		}
		data, ok := key.Bytes(emu.ModeBracketedPaste, emu.KeyLegacy)
		assert.True(t, ok)
		assert.Equal(t, []byte("a"), data)
	})

	// Empty text should not be wrapped (nothing to paste)
	t.Run("empty text with bracketed paste mode", func(t *testing.T) {
		emptyKey := Key{
			Code: KeyText,
			Text: "",
		}
		data, ok := emptyKey.Bytes(emu.ModeBracketedPaste, emu.KeyLegacy)
		assert.True(t, ok)
		// Empty text is not wrapped - wrapping makes no sense for empty paste
		assert.NotContains(t, string(data), "\x1b[200~")
	})

	// Text with newlines should be wrapped
	t.Run("text with newlines and bracketed paste mode", func(t *testing.T) {
		multilineKey := Key{
			Code: KeyText,
			Text: "line1\nline2\nline3",
		}
		data, ok := multilineKey.Bytes(
			emu.ModeBracketedPaste,
			emu.KeyLegacy,
		)
		assert.True(t, ok)
		assert.Equal(
			t,
			[]byte("\x1b[200~line1\nline2\nline3\x1b[201~"),
			data,
		)
	})
}

func TestHasIncompleteBracketedPaste(t *testing.T) {
	// Complete paste is not incomplete
	assert.False(t, HasIncompleteBracketedPaste(
		[]byte("\x1b[200~hello\x1b[201~"),
	))

	// Start marker without end marker is incomplete
	assert.True(t, HasIncompleteBracketedPaste(
		[]byte("\x1b[200~hello"),
	))

	// No paste markers at all
	assert.False(t, HasIncompleteBracketedPaste(
		[]byte("hello world"),
	))

	// Just the start marker
	assert.True(t, HasIncompleteBracketedPaste(
		[]byte("\x1b[200~"),
	))

	// Data before the start marker, no end
	assert.True(t, HasIncompleteBracketedPaste(
		[]byte("abc\x1b[200~hello"),
	))

	// Data before the start marker, with end
	assert.False(t, HasIncompleteBracketedPaste(
		[]byte("abc\x1b[200~hello\x1b[201~"),
	))
}
