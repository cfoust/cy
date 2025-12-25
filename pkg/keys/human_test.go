package keys

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestFromHuman(t *testing.T) {
	type fromCase struct {
		input string
		key   Key
		ok    bool
	}

	cases := []fromCase{
		{
			"test",
			Key{},
			false,
		},
		{
			"esc",
			Key{
				Code: KittyKeyEscape,
			},
			true,
		},
		{
			"ctrl+esc",
			Key{
				Code: KittyKeyEscape,
				Mod:  KeyModCtrl,
			},
			true,
		},
		{
			"ctrl+a",
			Key{
				Code:    'a',
				Shifted: 'A',
				Mod:     KeyModCtrl,
				Text:    "a",
			},
			true,
		},
		{
			"alt+ctrl+a",
			Key{
				Code:    'a',
				Shifted: 'A',
				Mod:     KeyModCtrl | KeyModAlt,
				Text:    "a",
			},
			true,
		},
		{
			"shift+a",
			Key{
				Code:    'a',
				Shifted: 'A',
				Mod:     KeyModShift,
				Text:    "A",
			},
			true,
		},
		{
			"a",
			Key{
				Code:    'a',
				Shifted: 'A',
				Text:    "a",
			},
			true,
		},
		{
			"Й",
			Key{
				Code:    'й',
				Shifted: 'Й',
				Mod:     KeyModShift,
				Text:    "Й",
			},
			true,
		},
		{
			"A",
			Key{
				Code:    'a',
				Shifted: 'A',
				Mod:     KeyModShift,
				Text:    "A",
			},
			true,
		},
		{
			"?",
			Key{
				Code:    '/',
				Shifted: '?',
				Mod:     KeyModShift,
				Text:    "?",
			},
			true,
		},
		{
			"ctrl+@",
			Key{
				Code:    '2',
				Shifted: '@',
				Mod:     KeyModCtrl | KeyModShift,
				Text:    "@",
			},
			true,
		},
		{
			"alt+",
			Key{},
			false,
		},
		{
			"t\x1b[?u",
			Key{},
			false,
		},
	}

	for _, test := range cases {
		t.Run(test.input, func(t *testing.T) {
			key, ok := FromHuman(test.input)
			assert.Equal(t, test.ok, ok)
			assert.Equal(
				t,
				test.key,
				key,
				"Incorrect translation from string to key",
			)
		})
	}
}

func TestToString(t *testing.T) {
	type toCase struct {
		key    Key
		output string
	}

	cases := []toCase{
		{
			Key{
				Code:    '2',
				Shifted: '@',
				Mod:     KeyModShift,
				Text:    "@",
			},
			"@",
		},
		{
			Key{
				Code:    'h',
				Shifted: 'H',
				Text:    "h",
			},
			"h",
		},
		{
			Key{
				Code:    'h',
				Shifted: 'H',
				Mod:     KeyModShift,
				Text:    "H",
			},
			"H",
		},
		{
			Key{
				Code:    'й',
				Shifted: 'Й',
				Mod:     KeyModShift,
				Text:    "Й",
			},
			"Й",
		},
		{
			Key{
				Code: 'a',
				Mod:  KeyModCtrl,
			},
			"ctrl+a",
		},
	}

	for _, test := range cases {
		t.Run(test.output, func(t *testing.T) {
			assert.Equal(
				t,
				test.output,
				test.key.String(),
				"Incorrect translation from key to string",
			)
		})
	}
}
