package keys

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestFromHuman(t *testing.T) {
	type nameCase struct {
		input string
		key   Key
		ok    bool
	}

	cases := []nameCase{
		{
			"test",
			Key{
				Code: KeyText,
				Text: "test",
			},
			true,
		},
		{
			"ctrl+a",
			Key{
				Code: 'a',
				Mod:  KeyModCtrl,
				Text: "a",
			},
			true,
		},
		{
			"alt+ctrl+a",
			Key{
				Code: 'a',
				Mod:  KeyModCtrl | KeyModAlt,
				Text: "a",
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
			"alt+",
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
