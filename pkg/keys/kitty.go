package keys

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"

	"github.com/cfoust/cy/pkg/emu"
)

var kittyRe = regexp.MustCompile(
	`^\x1b\[(?P<code>\d+)(:(?P<shifted>\d*)(:(?P<base>\d*))?)?(;(?P<modifier>\d+)?(:(?P<type>\d+)?)?(;(?P<text>(\d+)(:\d+)*))?)?u`,
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

// irregularCSI maps key constants to their irregular CSI sequences
// (i.e., those that don't follow the standard "ESC[{number}u" format)
var irregularCSI = map[rune]string{
	// Navigation keys with letter suffixes
	KittyKeyLeft:  "1D",
	KittyKeyRight: "1C",
	KittyKeyUp:    "1A",
	KittyKeyDown:  "1B",
	KittyKeyHome:  "1H", // Alternative: "7~"
	KittyKeyEnd:   "1F", // Alternative: "8~"
	KittyKPBegin:  "1E", // Alternative: "57427 ~"

	// Keys with ~ suffix
	KittyKeyInsert:   "2~",
	KittyKeyDelete:   "3~",
	KittyKeyPageUp:   "5~",
	KittyKeyPageDown: "6~",

	// Function keys F1-F4 with letter suffixes
	KittyKeyF1: "1P", // Alternative: "11~"
	KittyKeyF2: "1Q", // Alternative: "12~"
	KittyKeyF4: "1S", // Alternative: "14~"

	// Function keys F3,F5-F12 with ~ suffix
	KittyKeyF3:  "13~",
	KittyKeyF5:  "15~",
	KittyKeyF6:  "17~",
	KittyKeyF7:  "18~",
	KittyKeyF8:  "19~",
	KittyKeyF9:  "20~",
	KittyKeyF10: "21~",
	KittyKeyF11: "23~",
	KittyKeyF12: "24~",
}

// kittySequence generates the Kitty protocol sequence for this key
func (k Key) kittySequence(protocol emu.KeyProtocol) string {
	var (
		keycode   = k.Code
		modifiers = int(k.Mod)
		eventType = int(k.Type)
		// Check if event types should be reported
		reportEventTypes = protocol&emu.KeyReportEventTypes != 0
		// Check if associated text should be reported
		reportText = protocol&emu.KeyReportAssociatedText != 0
		// Determine if we should include event type in output
		includeEventType = reportEventTypes && eventType != 0
	)

	// Determine if we should include text in output
	var text string
	if len(k.Text) > 0 {
		text = k.Text
	}
	includeText := reportText && text != ""

	if includeEventType || includeText {
		// Extended format with event type and/or text
		if includeText {
			return fmt.Sprintf(
				"\x1b[%d;%d;%d;%su",
				keycode,
				modifiers,
				eventType,
				text,
			)
		} else {
			return fmt.Sprintf("\x1b[%d;%d;%du", keycode, modifiers, eventType)
		}
	} else if modifiers != 0 {
		// Standard format with modifiers
		return fmt.Sprintf("\x1b[%d;%du", keycode, modifiers)
	} else {
		// Minimal format
		return fmt.Sprintf("\x1b[%du", keycode)
	}
}

var (
	invalidKittyErr = fmt.Errorf("not a valid Kitty sequence")
)

func getInt(b []byte) (result int) {
	if len(b) == 0 {
		return 0
	}

	result, _ = strconv.Atoi(string(b))
	return
}

// parseKittySequence parses a Kitty protocol key sequence
// Format: ESC [ {keycode} [; {modifiers} [; {event_type} [; {text}]]] u
func parseKittySequence(b []byte) (key Key, width int, err error) {
	err = invalidKittyErr

	matches := kittyRe.FindSubmatch(b)
	if len(matches) == 0 {
		return
	}

	var (
		code     = getInt(matches[kittyRe.SubexpIndex("code")])
		shifted  = getInt(matches[kittyRe.SubexpIndex("shifted")])
		base     = getInt(matches[kittyRe.SubexpIndex("base")])
		modifier = getInt(matches[kittyRe.SubexpIndex("modifier")])
		type_    = getInt(matches[kittyRe.SubexpIndex("type")])
		// might be several code points
		text = string(matches[kittyRe.SubexpIndex("text")])
	)

	key.Code = rune(code)
	key.Shifted = rune(shifted)
	key.Base = rune(base)

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

	if len(text) > 0 {
		r := []rune{}
		for s := range strings.SplitSeq(text, ":") {
			num, _ := strconv.Atoi(string(s))
			r = append(r, rune(num))
		}
		key.Text = string(r)
	}

	width = len(matches[0])
	err = nil
	return
}

// GenerateKittyEnableSequence generates the escape sequence to enable Kitty protocol
func GenerateKittyEnableSequence(flags emu.KeyProtocol) string {
	return fmt.Sprintf("\x1b[>%du", int(flags))
}

// GenerateKittyDisableSequence generates the escape sequence to disable Kitty protocol
func GenerateKittyDisableSequence() string {
	return "\x1b[<u"
}
