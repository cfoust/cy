package keys

import (
	"strings"
)

var (
	humanKeys = map[string]rune{
		"escape":    KittyKeyEscape,
		"esc":       KittyKeyEscape,
		"enter":     KittyKeyEnter,
		"return":    KittyKeyEnter,
		"tab":       KittyKeyTab,
		"backspace": KittyKeyBackspace,
		"insert":    KittyKeyInsert,
		"delete":    KittyKeyDelete,
		"home":      KittyKeyHome,
		"end":       KittyKeyEnd,
		"pgup":      KittyKeyPageUp,
		"pgdown":    KittyKeyPageDown,
		"left":      KittyKeyLeft,
		"up":        KittyKeyUp,
		"right":     KittyKeyRight,
		"down":      KittyKeyDown,
		"f1":        KittyKeyF1,
		"f2":        KittyKeyF2,
		"f3":        KittyKeyF3,
		"f4":        KittyKeyF4,
		"f5":        KittyKeyF5,
		"f6":        KittyKeyF6,
		"f7":        KittyKeyF7,
		"f8":        KittyKeyF8,
		"f9":        KittyKeyF9,
		"f10":       KittyKeyF10,
		"f11":       KittyKeyF11,
		"f12":       KittyKeyF12,
		"space":     ' ',
	}

	humanModifiers = map[string]KeyModifiers{
		"alt":   KeyModAlt,
		"ctrl":  KeyModCtrl,
		"shift": KeyModShift,
		"super": KeyModSuper,
		"hyper": KeyModHyper,
		"meta":  KeyModMeta,
	}
)

// FromHuman translates human-readable key specifiers (such as "ctrl+a", "up",
// etc) into Keys. Unrecognized strings are represented as regular characters.
func FromHuman(human string) (key Key, ok bool) {
	var (
		modifiers KeyModifiers
		hadMod    = false
	)

	for {
		for name, mod := range humanModifiers {
			if !strings.HasPrefix(human, name+"+") {
				continue
			}

			// String ended
			if len(name)+1 == len(human) {
				return
			}

			human = human[len(name)+1:]
			modifiers |= mod
			hadMod = true
		}

		if !hadMod {
			break
		}

		hadMod = false
	}

	if hardcoded, ok := humanKeys[human]; ok {
		return Key{
			Code: hardcoded,
			Mod:  modifiers,
		}, true
	}

	var (
		i             = 0
		b             = []byte(human)
		msg, numRead  = Read(b[i:])
		keyMsg, isKey = msg.(Key)
	)

	// We don't want partial reads to produce valid keys
	if numRead < len(b) || !isKey {
		return
	}

	keyMsg.Mod |= modifiers
	return keyMsg, true
}
