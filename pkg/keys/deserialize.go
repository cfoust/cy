package keys

import (
	"fmt"
	"regexp"
	"sort"
	"unicode/utf8"
)

func k(r rune) Key {
	return Key{
		Code: r,
	}
}

func kMod(r rune, m KeyModifiers) Key {
	return Key{
		Code: r,
		Mod:  m,
	}
}

// Sequence mappings.
var sequences = map[string]Key{
	// Arrow keys
	"\x1b[A":    k(KittyKeyUp),
	"\x1b[B":    k(KittyKeyDown),
	"\x1b[C":    k(KittyKeyRight),
	"\x1b[D":    k(KittyKeyLeft),
	"\x1b[1;2A": kMod(KittyKeyUp, KeyModShift),
	"\x1b[1;2B": kMod(KittyKeyDown, KeyModShift),
	"\x1b[1;2C": kMod(KittyKeyRight, KeyModShift),
	"\x1b[1;2D": kMod(KittyKeyLeft, KeyModShift),
	"\x1b[OA":   kMod(KittyKeyUp, KeyModShift),    // DECCKM
	"\x1b[OB":   kMod(KittyKeyDown, KeyModShift),  // DECCKM
	"\x1b[OC":   kMod(KittyKeyRight, KeyModShift), // DECCKM
	"\x1b[OD":   kMod(KittyKeyLeft, KeyModShift),  // DECCKM
	"\x1b[a":    kMod(KittyKeyUp, KeyModShift),    // urxvt
	"\x1b[b":    kMod(KittyKeyDown, KeyModShift),  // urxvt
	"\x1b[c":    kMod(KittyKeyRight, KeyModShift), // urxvt
	"\x1b[d":    kMod(KittyKeyLeft, KeyModShift),  // urxvt
	"\x1b[1;3A": kMod(KittyKeyUp, KeyModAlt),
	"\x1b[1;3B": kMod(KittyKeyDown, KeyModAlt),
	"\x1b[1;3C": kMod(KittyKeyRight, KeyModAlt),
	"\x1b[1;3D": kMod(KittyKeyLeft, KeyModAlt),

	"\x1b[1;4A": kMod(KittyKeyUp, KeyModShift|KeyModAlt),
	"\x1b[1;4B": kMod(KittyKeyDown, KeyModShift|KeyModAlt),
	"\x1b[1;4C": kMod(KittyKeyRight, KeyModShift|KeyModAlt),
	"\x1b[1;4D": kMod(KittyKeyLeft, KeyModShift|KeyModAlt),

	"\x1b[1;5A": kMod(KittyKeyUp, KeyModCtrl),
	"\x1b[1;5B": kMod(KittyKeyDown, KeyModCtrl),
	"\x1b[1;5C": kMod(KittyKeyRight, KeyModCtrl),
	"\x1b[1;5D": kMod(KittyKeyLeft, KeyModCtrl),
	"\x1b[Oa":   kMod(KittyKeyUp, KeyModCtrl|KeyModAlt),    // urxvt
	"\x1b[Ob":   kMod(KittyKeyDown, KeyModCtrl|KeyModAlt),  // urxvt
	"\x1b[Oc":   kMod(KittyKeyRight, KeyModCtrl|KeyModAlt), // urxvt
	"\x1b[Od":   kMod(KittyKeyLeft, KeyModCtrl|KeyModAlt),  // urxvt
	"\x1b[1;6A": kMod(KittyKeyUp, KeyModCtrl|KeyModShift),
	"\x1b[1;6B": kMod(KittyKeyDown, KeyModCtrl|KeyModShift),
	"\x1b[1;6C": kMod(KittyKeyRight, KeyModCtrl|KeyModShift),
	"\x1b[1;6D": kMod(KittyKeyLeft, KeyModCtrl|KeyModShift),
	"\x1b[1;7A": kMod(KittyKeyUp, KeyModCtrl|KeyModAlt),
	"\x1b[1;7B": kMod(KittyKeyDown, KeyModCtrl|KeyModAlt),
	"\x1b[1;7C": kMod(KittyKeyRight, KeyModCtrl|KeyModAlt),
	"\x1b[1;7D": kMod(KittyKeyLeft, KeyModCtrl|KeyModAlt),
	"\x1b[1;8A": kMod(KittyKeyUp, KeyModCtrl|KeyModShift|KeyModAlt),
	"\x1b[1;8B": kMod(KittyKeyDown, KeyModCtrl|KeyModShift|KeyModAlt),
	"\x1b[1;8C": kMod(KittyKeyRight, KeyModCtrl|KeyModShift|KeyModAlt),
	"\x1b[1;8D": kMod(KittyKeyLeft, KeyModCtrl|KeyModShift|KeyModAlt),

	// Miscellaneous keys
	"\x1b[Z": kMod(KittyKeyTab, KeyModShift),

	"\x1b[2~":   k(KittyKeyInsert),
	"\x1b[3;2~": kMod(KittyKeyInsert, KeyModAlt),

	"\x1b[3~":   k(KittyKeyDelete),
	"\x1b[3;3~": kMod(KittyKeyDelete, KeyModAlt),

	"\x1b[5~":   k(KittyKeyPageUp),
	"\x1b[5;3~": kMod(KittyKeyPageUp, KeyModAlt),
	"\x1b[5;5~": kMod(KittyKeyPageUp, KeyModCtrl),
	"\x1b[5^":   kMod(KittyKeyPageUp, KeyModCtrl), // urxvt
	"\x1b[5;7~": kMod(KittyKeyPageUp, KeyModCtrl|KeyModAlt),

	"\x1b[6~":   k(KittyKeyPageDown),
	"\x1b[6;3~": kMod(KittyKeyPageDown, KeyModAlt),
	"\x1b[6;5~": kMod(KittyKeyPageDown, KeyModCtrl),
	"\x1b[6^":   kMod(KittyKeyPageDown, KeyModCtrl), // urxvt
	"\x1b[6;7~": kMod(KittyKeyPageDown, KeyModCtrl|KeyModAlt),

	"\x1b[1~": k(KittyKeyHome),
	"\x1b[H": k(
		KittyKeyHome,
	), // xterm, lxterm
	"\x1b[1;3H": kMod(
		KittyKeyHome,
		KeyModAlt,
	), // xterm, lxterm
	"\x1b[1;5H": kMod(
		KittyKeyHome,
		KeyModCtrl,
	), // xterm, lxterm
	"\x1b[1;7H": kMod(
		KittyKeyHome,
		KeyModCtrl|KeyModAlt,
	), // xterm, lxterm
	"\x1b[1;2H": kMod(
		KittyKeyHome,
		KeyModShift,
	), // xterm, lxterm
	"\x1b[1;4H": kMod(
		KittyKeyHome,
		KeyModShift|KeyModAlt,
	), // xterm, lxterm
	"\x1b[1;6H": kMod(
		KittyKeyHome,
		KeyModCtrl|KeyModShift,
	), // xterm, lxterm
	"\x1b[1;8H": kMod(
		KittyKeyHome,
		KeyModCtrl|KeyModShift|KeyModAlt,
	), // xterm, lxterm

	"\x1b[4~": k(KittyKeyEnd),
	"\x1b[F": k(
		KittyKeyEnd,
	), // xterm, lxterm
	"\x1b[1;3F": kMod(
		KittyKeyEnd,
		KeyModAlt,
	), // xterm, lxterm
	"\x1b[1;5F": kMod(
		KittyKeyEnd,
		KeyModCtrl,
	), // xterm, lxterm
	"\x1b[1;7F": kMod(
		KittyKeyEnd,
		KeyModCtrl|KeyModAlt,
	), // xterm, lxterm
	"\x1b[1;2F": kMod(
		KittyKeyEnd,
		KeyModShift,
	), // xterm, lxterm
	"\x1b[1;4F": kMod(
		KittyKeyEnd,
		KeyModShift|KeyModAlt,
	), // xterm, lxterm
	"\x1b[1;6F": kMod(
		KittyKeyEnd,
		KeyModCtrl|KeyModShift,
	), // xterm, lxterm
	"\x1b[1;8F": kMod(
		KittyKeyEnd,
		KeyModCtrl|KeyModShift|KeyModAlt,
	), // xterm, lxterm

	"\x1b[7~": k(KittyKeyHome),                            // urxvt
	"\x1b[7^": kMod(KittyKeyHome, KeyModCtrl),             // urxvt
	"\x1b[7$": kMod(KittyKeyHome, KeyModShift),            // urxvt
	"\x1b[7@": kMod(KittyKeyHome, KeyModCtrl|KeyModShift), // urxvt

	"\x1b[8~": k(KittyKeyEnd),                            // urxvt
	"\x1b[8^": kMod(KittyKeyEnd, KeyModCtrl),             // urxvt
	"\x1b[8$": kMod(KittyKeyEnd, KeyModShift),            // urxvt
	"\x1b[8@": kMod(KittyKeyEnd, KeyModCtrl|KeyModShift), // urxvt

	// Function keys, Linux console
	"\x1b[[A": k(KittyKeyF1), // linux console
	"\x1b[[B": k(KittyKeyF2), // linux console
	"\x1b[[C": k(KittyKeyF3), // linux console
	"\x1b[[D": k(KittyKeyF4), // linux console
	"\x1b[[E": k(KittyKeyF5), // linux console

	// Function keys, X11
	"\x1bOP": k(KittyKeyF1), // vt100, xterm
	"\x1bOQ": k(KittyKeyF2), // vt100, xterm
	"\x1bOR": k(KittyKeyF3), // vt100, xterm
	"\x1bOS": k(KittyKeyF4), // vt100, xterm

	"\x1b[1;3P": kMod(KittyKeyF1, KeyModAlt), // vt100, xterm
	"\x1b[1;3Q": kMod(KittyKeyF2, KeyModAlt), // vt100, xterm
	"\x1b[1;3R": kMod(KittyKeyF3, KeyModAlt), // vt100, xterm
	"\x1b[1;3S": kMod(KittyKeyF4, KeyModAlt), // vt100, xterm

	"\x1b[11~": k(KittyKeyF1), // urxvt
	"\x1b[12~": k(KittyKeyF2), // urxvt
	"\x1b[13~": k(KittyKeyF3), // urxvt
	"\x1b[14~": k(KittyKeyF4), // urxvt

	"\x1b[15~": k(KittyKeyF5), // vt100, xterm, also urxvt

	"\x1b[15;3~": kMod(KittyKeyF5, KeyModAlt), // vt100, xterm, also urxvt

	"\x1b[17~": k(KittyKeyF6),  // vt100, xterm, also urxvt
	"\x1b[18~": k(KittyKeyF7),  // vt100, xterm, also urxvt
	"\x1b[19~": k(KittyKeyF8),  // vt100, xterm, also urxvt
	"\x1b[20~": k(KittyKeyF9),  // vt100, xterm, also urxvt
	"\x1b[21~": k(KittyKeyF10), // vt100, xterm, also urxvt

	"\x1b[17;3~": kMod(KittyKeyF6, KeyModAlt),  // vt100, xterm
	"\x1b[18;3~": kMod(KittyKeyF7, KeyModAlt),  // vt100, xterm
	"\x1b[19;3~": kMod(KittyKeyF8, KeyModAlt),  // vt100, xterm
	"\x1b[20;3~": kMod(KittyKeyF9, KeyModAlt),  // vt100, xterm
	"\x1b[21;3~": kMod(KittyKeyF10, KeyModAlt), // vt100, xterm

	"\x1b[23~": k(KittyKeyF11), // vt100, xterm, also urxvt
	"\x1b[24~": k(KittyKeyF12), // vt100, xterm, also urxvt

	"\x1b[23;3~": kMod(KittyKeyF11, KeyModAlt), // vt100, xterm
	"\x1b[24;3~": kMod(KittyKeyF12, KeyModAlt), // vt100, xterm

	"\x1b[1;2P": k(0xE000 + 124), // F13
	"\x1b[1;2Q": k(0xE000 + 125), // F14

	"\x1b[25~": k(0xE000 + 124), // F13, vt100, xterm, also urxvt
	"\x1b[26~": k(0xE000 + 125), // F14, vt100, xterm, also urxvt

	"\x1b[25;3~": kMod(0xE000+124, KeyModAlt), // F13, vt100, xterm
	"\x1b[26;3~": kMod(0xE000+125, KeyModAlt), // F14, vt100, xterm

	"\x1b[1;2R": k(0xE000 + 126), // F15
	"\x1b[1;2S": k(0xE000 + 127), // F16

	"\x1b[28~": k(0xE000 + 126), // F15, vt100, xterm, also urxvt
	"\x1b[29~": k(0xE000 + 127), // F16, vt100, xterm, also urxvt

	"\x1b[28;3~": kMod(0xE000+126, KeyModAlt), // F15, vt100, xterm
	"\x1b[29;3~": kMod(0xE000+127, KeyModAlt), // F16, vt100, xterm

	"\x1b[15;2~": k(0xE000 + 128), // F17
	"\x1b[17;2~": k(0xE000 + 129), // F18
	"\x1b[18;2~": k(0xE000 + 130), // F19
	"\x1b[19;2~": k(0xE000 + 131), // F20

	"\x1b[31~": k(0xE000 + 128), // F17
	"\x1b[32~": k(0xE000 + 129), // F18
	"\x1b[33~": k(0xE000 + 130), // F19
	"\x1b[34~": k(0xE000 + 131), // F20

	// Powershell sequences.
	"\x1bOA": k(KittyKeyUp),
	"\x1bOB": k(KittyKeyDown),
	"\x1bOC": k(KittyKeyRight),
	"\x1bOD": k(KittyKeyLeft),
}

// Control keys. We could do this with an iota, but the values are very
// specific, so we set the values explicitly to avoid any confusion.
//
// See also:
// https://en.wikipedia.org/wiki/C0_and_C1_control_codes
const (
	keyNUL rune = 0   // null, \0
	keySOH rune = 1   // start of heading
	keySTX rune = 2   // start of text
	keyETX rune = 3   // break, ctrl+c
	keyEOT rune = 4   // end of transmission
	keyENQ rune = 5   // enquiry
	keyACK rune = 6   // acknowledge
	keyBEL rune = 7   // bell, \a
	keyBS  rune = 8   // backspace
	keyHT  rune = 9   // horizontal tabulation, \t
	keyLF  rune = 10  // line feed, \n
	keyVT  rune = 11  // vertical tabulation \v
	keyFF  rune = 12  // form feed \f
	keyCR  rune = 13  // carriage return, \r
	keySO  rune = 14  // shift out
	keySI  rune = 15  // shift in
	keyDLE rune = 16  // data link escape
	keyDC1 rune = 17  // device control one
	keyDC2 rune = 18  // device control two
	keyDC3 rune = 19  // device control three
	keyDC4 rune = 20  // device control four
	keyNAK rune = 21  // negative acknowledge
	keySYN rune = 22  // synchronous idle
	keyETB rune = 23  // end of transmission block
	keyCAN rune = 24  // cancel
	keyEM  rune = 25  // end of medium
	keySUB rune = 26  // substitution
	keyESC rune = 27  // escape, \e
	keyFS  rune = 28  // file separator
	keyGS  rune = 29  // group separator
	keyRS  rune = 30  // record separator
	keyUS  rune = 31  // unit separator
	keyDEL rune = 127 // delete. on most systems this is mapped to backspace, I hear
)

var controlToKey = map[rune]Key{
	// keyHT: kMod('j', KeyModCtrl) | k(KittyKeyTab)
	// keyCR: kMod('m', KeyModCtrl) | k(KittyKeyEnter)
	// keyESC: kMod('[', KeyModCtrl) | k(KittyKeyEscape)
	// keyDEL: kMod('?', KeyModCtrl) | k(KittyKeyBackspace)
	keyCR:  k(KittyKeyEnter),
	keyDEL: k(KittyKeyBackspace),
	keyHT:  k(KittyKeyTab),
	keyESC: k(KittyKeyEscape),
	keyNUL: kMod('@', KeyModCtrl),
	keySOH: kMod('a', KeyModCtrl),
	keySTX: kMod('b', KeyModCtrl),
	keyETX: kMod('c', KeyModCtrl),
	keyEOT: kMod('d', KeyModCtrl),
	keyENQ: kMod('e', KeyModCtrl),
	keyACK: kMod('f', KeyModCtrl),
	keyBEL: kMod('g', KeyModCtrl),
	keyBS:  kMod('h', KeyModCtrl),
	keyLF:  kMod('j', KeyModCtrl),
	keyVT:  kMod('k', KeyModCtrl),
	keyFF:  kMod('l', KeyModCtrl),
	keySO:  kMod('n', KeyModCtrl),
	keySI:  kMod('o', KeyModCtrl),
	keyDLE: kMod('p', KeyModCtrl),
	keyDC1: kMod('q', KeyModCtrl),
	keyDC2: kMod('r', KeyModCtrl),
	keyDC3: kMod('s', KeyModCtrl),
	keyDC4: kMod('t', KeyModCtrl),
	keyNAK: kMod('u', KeyModCtrl),
	keySYN: kMod('v', KeyModCtrl),
	keyETB: kMod('w', KeyModCtrl),
	keyCAN: kMod('x', KeyModCtrl),
	keyEM:  kMod('y', KeyModCtrl),
	keySUB: kMod('z', KeyModCtrl),
	keyFS:  kMod('\\', KeyModCtrl),
	keyGS:  kMod(']', KeyModCtrl),
	keyRS:  kMod('^', KeyModCtrl),
	keyUS:  kMod('_', KeyModCtrl),
}

// UnknownInputEvent is reported by the input reader when an invalid utf-8
// byte is detected on the input. Currently, it is not handled further by keys.
// However, having this event makes it possible to troubleshoot invalid inputs.
type UnknownInputEvent byte

func (u UnknownInputEvent) String() string {
	return fmt.Sprintf("?%#02x?", int(u))
}

// UnknownCSISequenceEvent is reported by the input reader when an
// unrecognized CSI sequence is detected on the input. Currently, it
// is not handled further by bubbletea. However, having this event
// makes it possible to troubleshoot invalid inputs.
type UnknownCSISequenceEvent []byte

func (u UnknownCSISequenceEvent) String() string {
	return fmt.Sprintf("?CSI%+v?", []byte(u)[2:])
}

var spaceRunes = []rune{' '}

var unknownCSIRe = regexp.MustCompile(
	`^\x1b\[[\x30-\x3f]*[\x20-\x2f]*[\x40-\x7e]`,
)

// extSequences is used by the map-based algorithm below. It contains
// the sequences plus their alternatives with an escape character
// prefixed, plus the control chars, plus the space.
// It does not contain the NUL character, which is handled specially
// by detectOneMsg.
var extSequences = func() map[string]Key {
	s := map[string]Key{}

	for seq, key := range sequences {
		s[seq] = key
		if key.Mod&KeyModAlt == 0 {
			key.Mod |= KeyModAlt
			s["\x1b"+seq] = key
		}
	}

	for i := keyNUL + 1; i <= keyDEL; i++ {
		if i == keyESC {
			continue
		}

		if key, ok := controlToKey[i]; ok {
			s[string([]byte{byte(i)})] = key
			altKey := key
			altKey.Mod |= KeyModAlt
			s[string([]byte{'\x1b', byte(i)})] = altKey
		}

		if i == keyUS {
			i = keyDEL - 1
		}
	}

	s[" "] = Key{Code: ' '}
	s["\x1b "] = Key{
		Code: ' ',
		Mod:   KeyModAlt,
	}
	s["\x1b\x1b"] = Key{
		Code: KittyKeyEscape,
		Mod:   KeyModAlt,
	}
	return s
}()

// seqLengths is the sizes of valid sequences, starting with the
// largest size.
var seqLengths = func() []int {
	sizes := map[int]struct{}{}
	for seq := range extSequences {
		sizes[len(seq)] = struct{}{}
	}
	lsizes := make([]int, 0, len(sizes))
	for sz := range sizes {
		lsizes = append(lsizes, sz)
	}
	sort.Slice(lsizes, func(i, j int) bool { return lsizes[i] > lsizes[j] })
	return lsizes
}()

// detectSequence uses a longest prefix match over the input
// sequence and a hash map.
func detectSequence(input []byte) (hasSeq bool, width int, event any) {
	seqs := extSequences
	for _, sz := range seqLengths {
		if sz > len(input) {
			continue
		}
		prefix := input[:sz]
		key, ok := seqs[string(prefix)]
		if ok {
			return true, sz, key
		}
	}
	// Is this an unknown CSI sequence?
	if loc := unknownCSIRe.FindIndex(input); loc != nil {
		return true, loc[1], UnknownCSISequenceEvent(input[:loc[1]])
	}

	return false, 0, nil
}

// Read attempts to read a key or mouse event.
// event can be one of:
// - Key
// - MouseEvent
// - UnknownCSISequenceEvent
// - UnknownInputEvent
func Read(b []byte) (event any, w int) {
	// Try Kitty protocol sequence first
	if isKittySequence(b) {
		key, width, err := parseKittySequence(b)
		if err == nil {
			return key, width
		}
	}

	// Detect mouse events.
	if isMouseEvent(b) {
		return parseX10MouseEvent(b), 6
	}

	// Detect escape sequence and control characters other than NUL,
	// possibly with an escape character in front to mark the Alt
	// modifier.
	var foundSeq bool
	foundSeq, w, event = detectSequence(b)
	if foundSeq {
		return
	}

	// No non-NUL control character or escape sequence.
	// If we are seeing at least an escape character, remember it for later below.
	alt := false
	i := 0
	if b[0] == '\x1b' {
		alt = true
		i++
	}

	// Are we seeing a standalone NUL? This is not handled by detectSequence().
	if i < len(b) && b[i] == 0 {
		modifiers := KeyModifiers(0)
		if alt {
			modifiers |= KeyModAlt
		}
		return Key{
			Code: 0,
			Mod:   modifiers,
			Type:  KeyEventPress,
		}, i + 1
	}

	// Find the longest sequence of runes that are not control
	// characters from this point.
	var runes []rune
	for rw := 0; i < len(b); i += rw {
		var r rune
		r, rw = utf8.DecodeRune(b[i:])
		if r == utf8.RuneError || r <= rune(keyUS) || r == rune(keyDEL) ||
			r == ' ' {
			// Rune errors are handled below; control characters and spaces will
			// be handled by detectSequence in the next call to detectOneMsg.
			break
		}
		runes = append(runes, r)
		if alt {
			// We only support a single rune after an escape alt modifier.
			i += rw
			break
		}
	}
	if len(runes) > 0 {
		modifiers := KeyModifiers(0)
		if alt {
			modifiers |= KeyModAlt
		}

		return Key{
			Code: runes[0],
			Mod:   modifiers,
		}, i
	}

	// We didn't find an escape sequence, nor a valid rune. Was this a
	// lone escape character at the end of the input?
	if alt && len(b) == 1 {
		return k(KittyKeyEscape), 1
	}

	// The character at the current position is neither an escape
	// sequence, a valid rune start or a sole escape character. Report
	// it as an invalid byte.
	return UnknownInputEvent(b[0]), 1
}
