package keys

import (
	"strings"
	"unicode"
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

	inverseHumanKeys = func() (result map[rune]string) {
		result = make(map[rune]string)
		for name, r := range humanKeys {
			result[r] = name
		}

		// handle duplicates
		result[KittyKeyEnter] = "enter"
		result[KittyKeyEscape] = "esc"

		return
	}()

	humanModifiers = map[string]KeyModifiers{
		"alt":   KeyModAlt,
		"ctrl":  KeyModCtrl,
		"shift": KeyModShift,
		"super": KeyModSuper,
		"hyper": KeyModHyper,
		"meta":  KeyModMeta,
	}

	shiftToUpper = map[rune]rune{
		'`':  '~',
		'1':  '!',
		'2':  '@',
		'3':  '#',
		'4':  '$',
		'5':  '%',
		'6':  '^',
		'7':  '&',
		'8':  '*',
		'9':  '(',
		'0':  ')',
		'-':  '_',
		'=':  '+',
		'[':  '{',
		']':  '}',
		'\\': '|',
		';':  ':',
		'\'': '"',
		',':  '<',
		'.':  '>',
		'/':  '?',
	}

	shiftToLower = func() (result map[rune]rune) {
		result = make(map[rune]rune)
		for name, r := range shiftToUpper {
			result[r] = name
		}
		return
	}()
)

func unshift(code rune) (normal, shifted rune, isShift, ok bool) {
	var (
		upper, isLowerShift = shiftToUpper[code]
		lower, isUpperShift = shiftToLower[code]
		isUpper             = unicode.IsUpper(code)
		isLower             = unicode.IsLower(code)
	)

	if isUpper || isUpperShift {
		if isUpper {
			lower = unicode.ToLower(code)
		}

		normal = lower
		shifted = code
		isShift = true
		ok = true
		return
	} else if isLower || isLowerShift {
		if isLower {
			upper = unicode.ToUpper(code)
		}

		normal = code
		shifted = upper
		ok = true
		return
	}

	return
}

// FromHuman translates human-readable key specifiers (such as "ctrl+a", "up",
// etc) into Keys.
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
		b             = []byte(human)
		msg, numRead  = Read(b)
		keyMsg, isKey = msg.(Key)
	)

	// We don't want partial reads to produce valid keys
	if numRead < len(b) || !isKey || keyMsg.Code == KeyText {
		return
	}

	keyMsg.Mod |= modifiers

	if normal, shifted, mod, ok := unshift(keyMsg.Code); ok {
		keyMsg.Code = normal
		keyMsg.Shifted = shifted

		if mod {
			keyMsg.Mod |= KeyModShift
		}
	}

	if keyMsg.Mod == KeyModShift {
		keyMsg.Text = string(keyMsg.Shifted)
	}

	return keyMsg, true
}

// String returns a friendly string representation for a key. It's safe (and
// encouraged) for use in key comparison.
//
//	k := Key{KeyCode: KittyKeyEnter}
//	fmt.Println(k)
//	// Output: enter
func (k Key) String() (str string) {
	var modParts []string

	if k.HasSuper() {
		modParts = append(modParts, "super")
	}
	if k.HasHyper() {
		modParts = append(modParts, "hyper")
	}
	if k.HasMeta() {
		modParts = append(modParts, "meta")
	}
	if k.HasCtrl() {
		modParts = append(modParts, "ctrl")
	}
	if k.HasAlt() {
		modParts = append(modParts, "alt")
	}
	if k.Shifted == 0 && k.HasShift() {
		modParts = append(modParts, "shift")
	}

	var (
		keyName   = k.Text
		modifiers string
	)

	if len(modParts) > 0 {
		modifiers = strings.Join(modParts, "+") + "+"
	}

	if hardcoded, haveHuman := inverseHumanKeys[k.Code]; haveHuman {
		keyName = hardcoded
	}

	if len(k.Text) == 0 {
		keyName = string(k.Code)
	}

	return modifiers + keyName
}
