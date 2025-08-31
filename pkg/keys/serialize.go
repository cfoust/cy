package keys

import (
	"fmt"

	"github.com/cfoust/cy/pkg/emu"
)

// xtermSequences is an authoritative list of the byte sequences for keys to
// be interpreted by xterm, because sequences above contains conflicting keys.
// We probably need a better approach to this.
var xtermSequences = map[string]Key{
	"\x1b[A":    k(KittyKeyUp),
	"\x1b[B":    k(KittyKeyDown),
	"\x1b[C":    k(KittyKeyRight),
	"\x1b[D":    k(KittyKeyLeft),
	"\x1b[1;2A": kMod(KittyKeyUp, KeyModShift),
	"\x1b[1;2B": kMod(KittyKeyDown, KeyModShift),
	"\x1b[1;2C": kMod(KittyKeyRight, KeyModShift),
	"\x1b[1;2D": kMod(KittyKeyLeft, KeyModShift),

	"\x1b[OA": kMod(KittyKeyUp, KeyModShift),    // DECCKM
	"\x1b[OB": kMod(KittyKeyDown, KeyModShift),  // DECCKM
	"\x1b[OC": kMod(KittyKeyRight, KeyModShift), // DECCKM
	"\x1b[OD": kMod(KittyKeyLeft, KeyModShift),  // DECCKM

	"\x1b[1;5A": kMod(KittyKeyUp, KeyModCtrl),
	"\x1b[1;5B": kMod(KittyKeyDown, KeyModCtrl),
	"\x1b[1;5C": kMod(KittyKeyRight, KeyModCtrl),
	"\x1b[1;5D": kMod(KittyKeyLeft, KeyModCtrl),

	"\x1b[5;5~": kMod(KittyKeyPageUp, KeyModCtrl),
	"\x1b[6;5~": kMod(KittyKeyPageDown, KeyModCtrl),

	"\x1b[H":    k(KittyKeyHome),                                      // xterm, lxterm
	"\x1b[1;3H": kMod(KittyKeyHome, KeyModAlt),                        // xterm, lxterm
	"\x1b[1;5H": kMod(KittyKeyHome, KeyModCtrl),                       // xterm, lxterm
	"\x1b[1;7H": kMod(KittyKeyHome, KeyModCtrl|KeyModAlt),             // xterm, lxterm
	"\x1b[1;2H": kMod(KittyKeyHome, KeyModShift),                      // xterm, lxterm
	"\x1b[1;4H": kMod(KittyKeyHome, KeyModShift|KeyModAlt),            // xterm, lxterm
	"\x1b[1;6H": kMod(KittyKeyHome, KeyModCtrl|KeyModShift),           // xterm, lxterm
	"\x1b[1;8H": kMod(KittyKeyHome, KeyModCtrl|KeyModShift|KeyModAlt), // xterm, lxterm

	"\x1b[F":    k(KittyKeyEnd),                                      // xterm, lxterm
	"\x1b[1;3F": kMod(KittyKeyEnd, KeyModAlt),                        // xterm, lxterm
	"\x1b[1;5F": kMod(KittyKeyEnd, KeyModCtrl),                       // xterm, lxterm
	"\x1b[1;7F": kMod(KittyKeyEnd, KeyModCtrl|KeyModAlt),             // xterm, lxterm
	"\x1b[1;2F": kMod(KittyKeyEnd, KeyModShift),                      // xterm, lxterm
	"\x1b[1;4F": kMod(KittyKeyEnd, KeyModShift|KeyModAlt),            // xterm, lxterm
	"\x1b[1;6F": kMod(KittyKeyEnd, KeyModCtrl|KeyModShift),           // xterm, lxterm
	"\x1b[1;8F": kMod(KittyKeyEnd, KeyModCtrl|KeyModShift|KeyModAlt), // xterm, lxterm

	"\x1bOP": k(KittyKeyF1), // vt100, xterm
	"\x1bOQ": k(KittyKeyF2), // vt100, xterm
	"\x1bOR": k(KittyKeyF3), // vt100, xterm
	"\x1bOS": k(KittyKeyF4), // vt100, xterm

	"\x1b[1;3P": kMod(KittyKeyF1, KeyModAlt), // vt100, xterm
	"\x1b[1;3Q": kMod(KittyKeyF2, KeyModAlt), // vt100, xterm
	"\x1b[1;3R": kMod(KittyKeyF3, KeyModAlt), // vt100, xterm
	"\x1b[1;3S": kMod(KittyKeyF4, KeyModAlt), // vt100, xterm

	"\x1b[28~": k(0xE000 + 126), // F15 - vt100, xterm, also urxvt
	"\x1b[29~": k(0xE000 + 127), // F16 - vt100, xterm, also urxvt
}

// inverseSequences is a mapping from a Key to its byte sequence.
var inverseSequences = func() map[keyLookup][]byte {
	s := map[keyLookup][]byte{}
	for str, key := range extSequences {
		s[key.toLookup()] = []byte(str)
	}

	for str, key := range xtermSequences {
		s[key.toLookup()] = []byte(str)
	}

	return s
}()


// legacyBytes returns the traditional byte encoding for a key
func (k Key) legacyBytes() (data []byte) {
	// Convert Kitty-based key to legacy format
	alt := k.Mod&KeyModAlt != 0

	if len(k.Runes) == 0 {
		return data
	}

	// If we have multiple runes, they're likely regular Unicode characters
	if len(k.Runes) > 1 {
		if alt {
			data = append(data, '\x1b')
		}
		data = append(data, []byte(string(k.Runes))...)
		return data
	}

	keyCode := k.Runes[0]

	// Handle space specially
	if keyCode == ' ' {
		data = append(data, []byte(" ")...)
		return data
	}

	// Handle regular Unicode characters
	if keyCode <= 0x10FFFF && keyCode > 31 && keyCode != 127 {
		if alt {
			data = append(data, '\x1b')
		}
		data = append(data, []byte(string(rune(keyCode)))...)
		return data
	}

	legacyType := k.toLegacyKeyType()
	if legacyType != 0 {
		if seq, ok := inverseSequences[keyLookup{
			Type: legacyType,
			Alt:  alt,
		}]; ok {
			data = append(data, seq...)
			return data
		}

		// ESC is special case
		if legacyType == keyESC {
			data = append(data, '\x1b')
			return data
		}
	}

	return data
}

func (k Key) Bytes(protocol emu.KeyProtocol) (data []byte) {
	if protocol != emu.KeyLegacy {
		return []byte(k.kittySequence(protocol))
	}

	return k.legacyBytes()
}
