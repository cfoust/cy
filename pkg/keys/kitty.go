package keys

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"

	"github.com/cfoust/cy/pkg/emu"
)

var (
	kittyRe = regexp.MustCompile(
		`^\x1b\[(?P<code>\d+)(:(?P<shifted>\d*)(:(?P<base>\d*))?)?(;(?P<modifier>\d+)?(:(?P<type>\d+)?)?(;(?P<text>(\d+)(:\d+)*))?)?u`,
	)

	kittyLegacy = regexp.MustCompile(
		`^\x1b\[(?P<code>\d+)?(;(?P<modifier>\d+)(:(?P<type>\d+)?)?)?(?P<suffix>[~ABCDEFHPQS])`,
	)
)

const (
	// The protocol defines special codes for these
	KittyKeyEscape    = 27
	KittyKeyEnter     = 13
	KittyKeyTab       = 9
	KittyKeyBackspace = 127

	// There are a bunch of keys that have irregular encodings; we use our
	// own internal codes for these
	/////////////////////////////////////////////////////////////////////
	// Navigation Keys
	KittyKeyHome     = 0xE000 + 1 // 1 H or 7 ~
	KittyKeyInsert   = 0xE000 + 2
	KittyKeyDelete   = 0xE000 + 3
	KittyKeyEnd      = 0xE000 + 4 // 1 F or 8 ~
	KittyKeyPageUp   = 0xE000 + 5
	KittyKeyPageDown = 0xE000 + 6
	KittyKeyUp       = 0xE000 + 65 // 1 A
	KittyKeyDown     = 0xE000 + 66 // 1 B
	KittyKeyRight    = 0xE000 + 67 // 1 C
	KittyKeyLeft     = 0xE000 + 68 // 1 D

	// Function Keys F1-F12
	KittyKeyF1  = 0xE000 + 112 // 1 P or 11 ~
	KittyKeyF2  = 0xE000 + 113 // 1 Q or 12 ~
	KittyKeyF3  = 0xE000 + 114 // 13 ~
	KittyKeyF4  = 0xE000 + 115 // 1 S or 14 ~
	KittyKeyF5  = 0xE000 + 116 // 15 ~
	KittyKeyF6  = 0xE000 + 117 // 17 ~
	KittyKeyF7  = 0xE000 + 118 // 18 ~
	KittyKeyF8  = 0xE000 + 119 // 19 ~
	KittyKeyF9  = 0xE000 + 120 // 20 ~
	KittyKeyF10 = 0xE000 + 121 // 21 ~
	KittyKeyF11 = 0xE000 + 122 // 23 ~
	KittyKeyF12 = 0xE000 + 123 // 24 ~
	/////////////////////////////////////////////////////////////////////

	// The rest of the keys all have fixed codes
	// Lock/System Keys
	KittyCapsLock    = 57358
	KittyScrollLock  = 57359
	KittyNumLock     = 57360
	KittyPrintScreen = 57361
	KittyPause       = 57362
	KittyMenu        = 57363

	// Function Keys F13-F35
	KittyKeyF13 = 57376
	KittyKeyF14 = 57377
	KittyKeyF15 = 57378
	KittyKeyF16 = 57379
	KittyKeyF17 = 57380
	KittyKeyF18 = 57381
	KittyKeyF19 = 57382
	KittyKeyF20 = 57383
	KittyKeyF21 = 57384
	KittyKeyF22 = 57385
	KittyKeyF23 = 57386
	KittyKeyF24 = 57387
	KittyKeyF25 = 57388
	KittyKeyF26 = 57389
	KittyKeyF27 = 57390
	KittyKeyF28 = 57391
	KittyKeyF29 = 57392
	KittyKeyF30 = 57393
	KittyKeyF31 = 57394
	KittyKeyF32 = 57395
	KittyKeyF33 = 57396
	KittyKeyF34 = 57397
	KittyKeyF35 = 57398

	// Keypad Keys
	KittyKP0         = 57399
	KittyKP1         = 57400
	KittyKP2         = 57401
	KittyKP3         = 57402
	KittyKP4         = 57403
	KittyKP5         = 57404
	KittyKP6         = 57405
	KittyKP7         = 57406
	KittyKP8         = 57407
	KittyKP9         = 57408
	KittyKPDecimal   = 57409
	KittyKPDivide    = 57410
	KittyKPMultiply  = 57411
	KittyKPSubtract  = 57412
	KittyKPAdd       = 57413
	KittyKPEnter     = 57414
	KittyKPEqual     = 57415
	KittyKPSeparator = 57416
	KittyKPLeft      = 57417
	KittyKPRight     = 57418
	KittyKPUp        = 57419
	KittyKPDown      = 57420
	KittyKPPageUp    = 57421
	KittyKPPageDown  = 57422
	KittyKPHome      = 57423
	KittyKPEnd       = 57424
	KittyKPInsert    = 57425
	KittyKPDelete    = 57426
	KittyKPBegin     = 57427

	// Media Keys
	KittyMediaPlay        = 57428
	KittyMediaPause       = 57429
	KittyMediaPlayPause   = 57430
	KittyMediaReverse     = 57431
	KittyMediaStop        = 57432
	KittyMediaFastForward = 57433
	KittyMediaRewind      = 57434
	KittyMediaTrackNext   = 57435
	KittyMediaTrackPrev   = 57436
	KittyMediaRecord      = 57437
	KittyLowerVolume      = 57438
	KittyRaiseVolume      = 57439
	KittyMuteVolume       = 57440

	// Modifier Keys
	KittyLeftShift    = 57441
	KittyLeftControl  = 57442
	KittyLeftAlt      = 57443
	KittyLeftSuper    = 57444
	KittyLeftHyper    = 57445
	KittyLeftMeta     = 57446
	KittyRightShift   = 57447
	KittyRightControl = 57448
	KittyRightAlt     = 57449
	KittyRightSuper   = 57450
	KittyRightHyper   = 57451
	KittyRightMeta    = 57452
)

func invertMap[S comparable, T comparable](in map[S]T) (out map[T]S) {
	out = make(map[T]S)
	for k, v := range in {
		out[v] = k
	}
	return
}

var (
	legacyLetter = map[string]rune{
		// Navigation keys with letter suffixes
		"D": KittyKeyLeft,
		"C": KittyKeyRight,
		"A": KittyKeyUp,
		"B": KittyKeyDown,
		"H": KittyKeyHome, // Alternative: "7~"
		"F": KittyKeyEnd,  // Alternative: "8~"
		"E": KittyKPBegin, // Alternative: "57427 ~"

		// Function keys F1-F4 with letter suffixes
		"P": KittyKeyF1, // Alternative: "11~"
		"Q": KittyKeyF2, // Alternative: "12~"
		"S": KittyKeyF4, // Alternative: "14~"
	}
	invertLegacyLetter = invertMap(legacyLetter)

	legacyNumber = map[int]rune{
		// Keys with ~ suffix
		2: KittyKeyInsert,
		3: KittyKeyDelete,
		5: KittyKeyPageUp,
		6: KittyKeyPageDown,

		// Function keys F3,F5-F12 with ~ suffix
		13: KittyKeyF3,
		15: KittyKeyF5,
		17: KittyKeyF6,
		18: KittyKeyF7,
		19: KittyKeyF8,
		20: KittyKeyF9,
		21: KittyKeyF10,
		23: KittyKeyF11,
		24: KittyKeyF12,
	}
	invertLegacyNumber = invertMap(legacyNumber)

	invalidKittyErr = fmt.Errorf("not a valid Kitty sequence")
)

func getInt(b []byte) (result int) {
	if len(b) == 0 {
		return 0
	}

	result, _ = strconv.Atoi(string(b))
	return
}

func parseModifier(modifier, type_ int, key *Key) {
	// In the escape code, the modifier value is encoded as a decimal number
	// which is 1 + actual modifiers
	if modifier > 0 {
		modifier--
	}

	if modifier >= 256 {
		modifier = 255
	}

	key.Mod = KeyModifiers(modifier)

	if type_ == 0 {
		type_ = 1
	}

	key.Type = KeyEventType(type_ - 1)
}

func parseKittyLegacy(match [][]byte) (key Key, width int, err error) {
	err = invalidKittyErr

	var (
		code     = getInt(match[kittyLegacy.SubexpIndex("code")])
		modifier = getInt(match[kittyLegacy.SubexpIndex("modifier")])
		type_    = getInt(match[kittyLegacy.SubexpIndex("type")])
		suffix   = string(match[kittyLegacy.SubexpIndex("suffix")])
	)

	if len(suffix) == 0 {
		return
	}

	if code == 0 {
		code = 1
	}

	switch suffix {
	case "~":
		r, ok := legacyNumber[code]
		if !ok {
			return
		}
		key.Code = r
	default:
		r, ok := legacyLetter[suffix]
		if !ok {
			return
		}
		key.Code = r
	}

	parseModifier(modifier, type_, &key)
	err = nil
	return
}

func parseKittyPrimary(match [][]byte) (key Key, width int, err error) {
	var (
		code     = getInt(match[kittyRe.SubexpIndex("code")])
		shifted  = getInt(match[kittyRe.SubexpIndex("shifted")])
		base     = getInt(match[kittyRe.SubexpIndex("base")])
		modifier = getInt(match[kittyRe.SubexpIndex("modifier")])
		type_    = getInt(match[kittyRe.SubexpIndex("type")])
		// might be several code points
		text = string(match[kittyRe.SubexpIndex("text")])
	)

	key.Code = rune(code)
	key.Shifted = rune(shifted)
	key.Base = rune(base)

	parseModifier(modifier, type_, &key)

	if len(text) > 0 {
		r := []rune{}
		for s := range strings.SplitSeq(text, ":") {
			num, _ := strconv.Atoi(string(s))
			r = append(r, rune(num))
		}
		key.Text = string(r)
	}

	err = nil
	return
}

// parseKittySequence parses a Kitty protocol key sequence
// Format: ESC [ {keycode} [; {modifiers} [; {event_type} [; {text}]]] u
func parseKittySequence(b []byte) (key Key, width int, err error) {
	err = invalidKittyErr

	if match := kittyLegacy.FindSubmatch(b); len(match) > 0 {
		width = len(match[0])
		return parseKittyLegacy(match)
	}

	if match := kittyRe.FindSubmatch(b); len(match) > 0 {
		width = len(match[0])
		return parseKittyPrimary(match)
	}

	return
}

type kittySequence struct {
	Code   rune
	Suffix string // u or [~ABCDEFHPQS]
}

// Kitty has a set of legacy text keys that require special handling
// The list of "legacy text keys" is defined here:
// https://sw.kovidgoyal.net/kitty/keyboard-protocol/#legacy-text
var legacyTextKeys = (func() (keys map[Key]bool) {
	keys = make(map[Key]bool)
	runes := []rune{
		'`',
		'-',
		'=',
		'[',
		']',
		'\\',
		';',
		'\'',
		',',
		'.',
		'/',
	}

	for i := '0'; i <= '9'; i++ {
		runes = append(runes, i)
	}

	for i := 'a'; i <= 'z'; i++ {
		runes = append(runes, i)
	}

	modifiers := []KeyModifiers{
		KeyModShift,
		KeyModAlt,
		KeyModCtrl,
		KeyModShift | KeyModAlt,
		KeyModCtrl | KeyModAlt,
	}

	for _, r := range runes {
		for _, mod := range modifiers {
			keys[Key{
				Code: r,
				Mod:  mod,
			}] = true
		}
	}

	return
}())

func isLegacyText(k Key) (yes bool) {
	_, yes = legacyTextKeys[Key{
		Code: k.Code,
		Mod:  k.Mod,
	}]
	return
}

func runeToCodepoint(r rune) string {
	return fmt.Sprintf("%d", r)
}

func colonSeparate(runes ...rune) string {
	s := make([]string, 0, len(runes))
	for _, r := range runes {
		// we want to omit zeroes, kitty omits ones
		if r == 0 || r == 1 {
			continue
		}

		s = append(s, runeToCodepoint(r))
	}

	return strings.Join(
		s,
		":",
	)
}

func kittyEncode(
	k Key,
	withEvent bool,
	withBase bool,
	withText bool,
) []byte {
	var (
		parts  = []string{}
		code   = k.Code
		suffix = "u"
	)

	if letter, ok := invertLegacyLetter[code]; ok {
		code = 1
		suffix = letter
	} else if number, ok := invertLegacyNumber[code]; ok {
		code = rune(number)
		suffix = "~"
	}

	codePoints := runeToCodepoint(code)
	if code == 1 {
		codePoints = ""
	} else if withBase && (k.Base != 0 || k.Shifted != 0) {
		codePoints = colonSeparate(
			code,
			k.Shifted,
			k.Base,
		)
	}

	parts = append(parts, codePoints)

	if withEvent {
		if k.Mod == 0 && k.Type == 0 {
			// If there's no text coming after this, don't add a
			// blank
			if withText {
				parts = append(parts, "")
			}
		} else {
			parts = append(parts, colonSeparate(
				rune(k.Mod+1),
				rune(k.Type+1),
			))
		}
	} else if k.Mod != 0 {
		parts = append(parts, runeToCodepoint(
			rune(k.Mod+1),
		))
	}

	if withText {
		// Modifier wasn't reported, but we're reporting text, so this
		// needs to be blank
		if len(parts) == 1 {
			parts = append(parts, "")
		}

		parts = append(parts, colonSeparate(
			[]rune(k.Text)...,
		))
	}

	return fmt.Appendf(
		[]byte{},
		"\x1b[%s%s",
		strings.Join(parts, ";"),
		suffix,
	)
}

// kittySequence generates the Kitty protocol sequence for this key
func (k Key) kittyBytes(protocol emu.KeyProtocol) (data []byte, ok bool) {
	var (
		haveDisambiguate = protocol&emu.KeyDisambiguate != 0
		haveTypes        = protocol&emu.KeyReportEventTypes != 0
		haveAlt          = protocol&emu.KeyReportAlternateKeys != 0
		haveText         = protocol&emu.KeyReportAssociatedText != 0
		haveAll          = protocol&emu.KeyReportAllKeys != 0
	)

	// We can only report press or repeat
	if !haveTypes && k.Type == KeyEventRelease {
		ok = false
		return
	}

	if haveAll || (haveDisambiguate && isLegacyText(k)) {
		return kittyEncode(
			k,
			haveTypes,
			haveAlt,
			haveText,
		), true
	}

	return k.legacyBytes()
}

// GenerateKittyEnableSequence generates the escape sequence to enable Kitty protocol
func GenerateKittyEnableSequence(flags emu.KeyProtocol) string {
	return fmt.Sprintf("\x1b[>%du", int(flags))
}

// GenerateKittyDisableSequence generates the escape sequence to disable Kitty protocol
func GenerateKittyDisableSequence() string {
	return "\x1b[<u"
}
