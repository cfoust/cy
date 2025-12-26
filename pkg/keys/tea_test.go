package keys

import (
	"testing"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/stretchr/testify/assert"
)

type teaCase struct {
	name     string
	key      Key
	expected tea.KeyMsg
}

func te(name string, key Key, msg tea.KeyMsg) teaCase {
	return teaCase{
		name:     name,
		key:      key,
		expected: msg,
	}
}

func TestTea(t *testing.T) {
	cases := []teaCase{
		te(
			"space",
			Key{
				Code: ' ',
				Text: " ",
			},
			tea.KeyMsg{
				Type:  tea.KeyRunes,
				Runes: []rune{' '},
			},
		),
		te(
			"lookup",
			Key{
				Code: ']',
				Mod:  KeyModCtrl,
			},
			tea.KeyMsg{
				Type: tea.KeyCtrlCloseBracket,
			},
		),
		te(
			"ctrl+c",
			Key{
				Code:    'c',
				Shifted: 'C',
				Mod:     KeyModCtrl,
			},
			tea.KeyMsg{
				Type: tea.KeyCtrlC,
			},
		),
		te(
			"shift+a",
			Key{
				Code:    'a',
				Shifted: 'A',
				Mod:     KeyModShift,
				Text:    "A",
			},
			tea.KeyMsg{
				Type:  tea.KeyRunes,
				Runes: []rune{'A'},
			},
		),
		te(
			"alt+shift+a",
			Key{
				Code:    'a',
				Shifted: 'A',
				Mod:     KeyModShift | KeyModAlt,
				Text:    "A",
			},
			tea.KeyMsg{
				Type:  tea.KeyRunes,
				Runes: []rune{'A'},
				Alt:   true,
			},
		),
		te(
			"zh",
			Key{
				Code: '致',
				Text: "致",
			},
			tea.KeyMsg{
				Type:  tea.KeyRunes,
				Runes: []rune{'致'},
			},
		),
		te(
			"repeat",
			Key{
				Code:    'a',
				Shifted: 'A',
				Text:    "a",
				Type:    KeyEventRepeat,
			},
			tea.KeyMsg{
				Type:  tea.KeyRunes,
				Runes: []rune{'a'},
			},
		),
		te(
			"repeat backspace",
			Key{
				Code: KittyKeyBackspace,
				Type: KeyEventRepeat,
			},
			tea.KeyMsg{
				Type: tea.KeyBackspace,
			},
		),
	}

	for _, test := range cases {
		t.Run(test.name, func(t *testing.T) {
			var (
				actual, _ = test.key.Tea()
			)
			assert.Equal(
				t,
				test.expected,
				actual,
				"Conversion to KeyMsg was incorrect",
			)
		})
	}
}
