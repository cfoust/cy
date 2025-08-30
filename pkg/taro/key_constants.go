package taro

// Control keys. We could do this with an iota, but the values are very
// specific, so we set the values explicitly to avoid any confusion.
//
// See also:
// https://en.wikipedia.org/wiki/C0_and_C1_control_codes
const (
	keyNUL KeyType = 0   // null, \0
	keySOH KeyType = 1   // start of heading
	keySTX KeyType = 2   // start of text
	keyETX KeyType = 3   // break, ctrl+c
	keyEOT KeyType = 4   // end of transmission
	keyENQ KeyType = 5   // enquiry
	keyACK KeyType = 6   // acknowledge
	keyBEL KeyType = 7   // bell, \a
	keyBS  KeyType = 8   // backspace
	keyHT  KeyType = 9   // horizontal tabulation, \t
	keyLF  KeyType = 10  // line feed, \n
	keyVT  KeyType = 11  // vertical tabulation \v
	keyFF  KeyType = 12  // form feed \f
	keyCR  KeyType = 13  // carriage return, \r
	keySO  KeyType = 14  // shift out
	keySI  KeyType = 15  // shift in
	keyDLE KeyType = 16  // data link escape
	keyDC1 KeyType = 17  // device control one
	keyDC2 KeyType = 18  // device control two
	keyDC3 KeyType = 19  // device control three
	keyDC4 KeyType = 20  // device control four
	keyNAK KeyType = 21  // negative acknowledge
	keySYN KeyType = 22  // synchronous idle
	keyETB KeyType = 23  // end of transmission block
	keyCAN KeyType = 24  // cancel
	keyEM  KeyType = 25  // end of medium
	keySUB KeyType = 26  // substitution
	keyESC KeyType = 27  // escape, \e
	keyFS  KeyType = 28  // file separator
	keyGS  KeyType = 29  // group separator
	keyRS  KeyType = 30  // record separator
	keyUS  KeyType = 31  // unit separator
	keyDEL KeyType = 127 // delete. on most systems this is mapped to backspace, I hear
)

// Control key aliases.
const (
	KeyNull      KeyType = keyNUL
	KeyBreak     KeyType = keyETX
	KeyEnter     KeyType = keyCR
	KeyBackspace KeyType = keyDEL
	KeyTab       KeyType = keyHT
	KeyEsc       KeyType = keyESC
	KeyEscape    KeyType = keyESC

	KeyCtrlAt           KeyType = keyNUL // ctrl+@
	KeyCtrlA            KeyType = keySOH
	KeyCtrlB            KeyType = keySTX
	KeyCtrlC            KeyType = keyETX
	KeyCtrlD            KeyType = keyEOT
	KeyCtrlE            KeyType = keyENQ
	KeyCtrlF            KeyType = keyACK
	KeyCtrlG            KeyType = keyBEL
	KeyCtrlH            KeyType = keyBS
	KeyCtrlI            KeyType = keyHT
	KeyCtrlJ            KeyType = keyLF
	KeyCtrlK            KeyType = keyVT
	KeyCtrlL            KeyType = keyFF
	KeyCtrlM            KeyType = keyCR
	KeyCtrlN            KeyType = keySO
	KeyCtrlO            KeyType = keySI
	KeyCtrlP            KeyType = keyDLE
	KeyCtrlQ            KeyType = keyDC1
	KeyCtrlR            KeyType = keyDC2
	KeyCtrlS            KeyType = keyDC3
	KeyCtrlT            KeyType = keyDC4
	KeyCtrlU            KeyType = keyNAK
	KeyCtrlV            KeyType = keySYN
	KeyCtrlW            KeyType = keyETB
	KeyCtrlX            KeyType = keyCAN
	KeyCtrlY            KeyType = keyEM
	KeyCtrlZ            KeyType = keySUB
	KeyCtrlOpenBracket  KeyType = keyESC // ctrl+[
	KeyCtrlBackslash    KeyType = keyFS  // ctrl+\
	KeyCtrlCloseBracket KeyType = keyGS  // ctrl+]
	KeyCtrlCaret        KeyType = keyRS  // ctrl+^
	KeyCtrlUnderscore   KeyType = keyUS  // ctrl+_
	KeyCtrlQuestionMark KeyType = keyDEL // ctrl+?
)

// Other keys.
const (
	KeyRunes KeyType = -(iota + 1)
	KeyUp
	KeyDown
	KeyRight
	KeyLeft
	KeyShiftTab
	KeyHome
	KeyEnd
	KeyPgUp
	KeyPgDown
	KeyCtrlPgUp
	KeyCtrlPgDown
	KeyDelete
	KeyInsert
	KeySpace
	KeyCtrlUp
	KeyCtrlDown
	KeyCtrlRight
	KeyCtrlLeft
	KeyCtrlHome
	KeyCtrlEnd
	KeyShiftUp
	KeyShiftDown
	KeyShiftRight
	KeyShiftLeft
	KeyShiftHome
	KeyShiftEnd
	KeyCtrlShiftUp
	KeyCtrlShiftDown
	KeyCtrlShiftLeft
	KeyCtrlShiftRight
	KeyCtrlShiftHome
	KeyCtrlShiftEnd
	KeyF1
	KeyF2
	KeyF3
	KeyF4
	KeyF5
	KeyF6
	KeyF7
	KeyF8
	KeyF9
	KeyF10
	KeyF11
	KeyF12
	KeyF13
	KeyF14
	KeyF15
	KeyF16
	KeyF17
	KeyF18
	KeyF19
	KeyF20
)

// Mappings for control keys and other special keys to friendly consts.
var keyNames = map[KeyType]string{
	// Control keys.
	keyNUL: "ctrl+@", // also ctrl+` (that's ctrl+backtick)
	keySOH: "ctrl+a",
	keySTX: "ctrl+b",
	keyETX: "ctrl+c",
	keyEOT: "ctrl+d",
	keyENQ: "ctrl+e",
	keyACK: "ctrl+f",
	keyBEL: "ctrl+g",
	keyBS:  "ctrl+h",
	keyHT:  "tab", // also ctrl+i
	keyLF:  "ctrl+j",
	keyVT:  "ctrl+k",
	keyFF:  "ctrl+l",
	keyCR:  "enter",
	keySO:  "ctrl+n",
	keySI:  "ctrl+o",
	keyDLE: "ctrl+p",
	keyDC1: "ctrl+q",
	keyDC2: "ctrl+r",
	keyDC3: "ctrl+s",
	keyDC4: "ctrl+t",
	keyNAK: "ctrl+u",
	keySYN: "ctrl+v",
	keyETB: "ctrl+w",
	keyCAN: "ctrl+x",
	keyEM:  "ctrl+y",
	keySUB: "ctrl+z",
	keyESC: "esc",
	keyFS:  "ctrl+\\",
	keyGS:  "ctrl+]",
	keyRS:  "ctrl+^",
	keyUS:  "ctrl+_",
	keyDEL: "backspace",

	// Other keys.
	KeyRunes:          "runes",
	KeyUp:             "up",
	KeyDown:           "down",
	KeyRight:          "right",
	KeySpace:          " ", // for backwards compatibility
	KeyLeft:           "left",
	KeyShiftTab:       "shift+tab",
	KeyHome:           "home",
	KeyEnd:            "end",
	KeyCtrlHome:       "ctrl+home",
	KeyCtrlEnd:        "ctrl+end",
	KeyShiftHome:      "shift+home",
	KeyShiftEnd:       "shift+end",
	KeyCtrlShiftHome:  "ctrl+shift+home",
	KeyCtrlShiftEnd:   "ctrl+shift+end",
	KeyPgUp:           "pgup",
	KeyPgDown:         "pgdown",
	KeyCtrlPgUp:       "ctrl+pgup",
	KeyCtrlPgDown:     "ctrl+pgdown",
	KeyDelete:         "delete",
	KeyInsert:         "insert",
	KeyCtrlUp:         "ctrl+up",
	KeyCtrlDown:       "ctrl+down",
	KeyCtrlRight:      "ctrl+right",
	KeyCtrlLeft:       "ctrl+left",
	KeyShiftUp:        "shift+up",
	KeyShiftDown:      "shift+down",
	KeyShiftRight:     "shift+right",
	KeyShiftLeft:      "shift+left",
	KeyCtrlShiftUp:    "ctrl+shift+up",
	KeyCtrlShiftDown:  "ctrl+shift+down",
	KeyCtrlShiftLeft:  "ctrl+shift+left",
	KeyCtrlShiftRight: "ctrl+shift+right",
	KeyF1:             "f1",
	KeyF2:             "f2",
	KeyF3:             "f3",
	KeyF4:             "f4",
	KeyF5:             "f5",
	KeyF6:             "f6",
	KeyF7:             "f7",
	KeyF8:             "f8",
	KeyF9:             "f9",
	KeyF10:            "f10",
	KeyF11:            "f11",
	KeyF12:            "f12",
	KeyF13:            "f13",
	KeyF14:            "f14",
	KeyF15:            "f15",
	KeyF16:            "f16",
	KeyF17:            "f17",
	KeyF18:            "f18",
	KeyF19:            "f19",
	KeyF20:            "f20",
}

// a mapping from human-readable string to KeyType
// there are some changes made to the mappings from keyNames, so it's not just
// a simple inversion
var keyRefs = map[string]KeyType{
	// Control keys.
	"ctrl+@":    keyNUL,
	"ctrl+a":    keySOH,
	"ctrl+b":    keySTX,
	"ctrl+c":    keyETX,
	"ctrl+d":    keyEOT,
	"ctrl+e":    keyENQ,
	"ctrl+f":    keyACK,
	"ctrl+g":    keyBEL,
	"ctrl+h":    keyBS,
	"tab":       keyHT,
	"ctrl+j":    keyLF,
	"ctrl+k":    keyVT,
	"ctrl+l":    keyFF,
	"enter":     keyCR,
	"return":    keyCR,
	"ctrl+n":    keySO,
	"ctrl+o":    keySI,
	"ctrl+p":    keyDLE,
	"ctrl+q":    keyDC1,
	"ctrl+r":    keyDC2,
	"ctrl+s":    keyDC3,
	"ctrl+t":    keyDC4,
	"ctrl+u":    keyNAK,
	"ctrl+v":    keySYN,
	"ctrl+w":    keyETB,
	"ctrl+x":    keyCAN,
	"ctrl+y":    keyEM,
	"ctrl+z":    keySUB,
	"esc":       keyESC,
	"ctrl+\\":   keyFS,
	"ctrl+]":    keyGS,
	"ctrl+^":    keyRS,
	"ctrl+_":    keyUS,
	"backspace": keyDEL,

	"up":               KeyUp,
	"down":             KeyDown,
	"right":            KeyRight,
	" ":                KeySpace, // for backwards compatibility
	"space":            KeySpace,
	"left":             KeyLeft,
	"shift+tab":        KeyShiftTab,
	"home":             KeyHome,
	"end":              KeyEnd,
	"ctrl+home":        KeyCtrlHome,
	"ctrl+end":         KeyCtrlEnd,
	"shift+home":       KeyShiftHome,
	"shift+end":        KeyShiftEnd,
	"ctrl+shift+home":  KeyCtrlShiftHome,
	"ctrl+shift+end":   KeyCtrlShiftEnd,
	"pgup":             KeyPgUp,
	"pgdown":           KeyPgDown,
	"ctrl+pgup":        KeyCtrlPgUp,
	"ctrl+pgdown":      KeyCtrlPgDown,
	"delete":           KeyDelete,
	"insert":           KeyInsert,
	"ctrl+up":          KeyCtrlUp,
	"ctrl+down":        KeyCtrlDown,
	"ctrl+right":       KeyCtrlRight,
	"ctrl+left":        KeyCtrlLeft,
	"shift+up":         KeyShiftUp,
	"shift+down":       KeyShiftDown,
	"shift+right":      KeyShiftRight,
	"shift+left":       KeyShiftLeft,
	"ctrl+shift+up":    KeyCtrlShiftUp,
	"ctrl+shift+down":  KeyCtrlShiftDown,
	"ctrl+shift+left":  KeyCtrlShiftLeft,
	"ctrl+shift+right": KeyCtrlShiftRight,
	"f1":               KeyF1,
	"f2":               KeyF2,
	"f3":               KeyF3,
	"f4":               KeyF4,
	"f5":               KeyF5,
	"f6":               KeyF6,
	"f7":               KeyF7,
	"f8":               KeyF8,
	"f9":               KeyF9,
	"f10":              KeyF10,
	"f11":              KeyF11,
	"f12":              KeyF12,
	"f13":              KeyF13,
	"f14":              KeyF14,
	"f15":              KeyF15,
	"f16":              KeyF16,
	"f17":              KeyF17,
	"f18":              KeyF18,
	"f19":              KeyF19,
	"f20":              KeyF20,
}

// Sequence mappings.
var sequences = map[string]Key{
	// Arrow keys
	"\x1b[A":    {KeyCode: KittyKeyUp, Modifiers: 0, EventType: KittyKeyPress},
	"\x1b[B":    {KeyCode: KittyKeyDown, Modifiers: 0, EventType: KittyKeyPress},
	"\x1b[C":    {KeyCode: KittyKeyRight, Modifiers: 0, EventType: KittyKeyPress},
	"\x1b[D":    {KeyCode: KittyKeyLeft, Modifiers: 0, EventType: KittyKeyPress},
	"\x1b[1;2A": {KeyCode: KittyKeyUp, Modifiers: KittyModShift, EventType: KittyKeyPress},
	"\x1b[1;2B": {KeyCode: KittyKeyDown, Modifiers: KittyModShift, EventType: KittyKeyPress},
	"\x1b[1;2C": {KeyCode: KittyKeyRight, Modifiers: KittyModShift, EventType: KittyKeyPress},
	"\x1b[1;2D": {KeyCode: KittyKeyLeft, Modifiers: KittyModShift, EventType: KittyKeyPress},
	"\x1b[OA":   {KeyCode: KittyKeyUp, Modifiers: KittyModShift, EventType: KittyKeyPress},    // DECCKM
	"\x1b[OB":   {KeyCode: KittyKeyDown, Modifiers: KittyModShift, EventType: KittyKeyPress},  // DECCKM
	"\x1b[OC":   {KeyCode: KittyKeyRight, Modifiers: KittyModShift, EventType: KittyKeyPress}, // DECCKM
	"\x1b[OD":   {KeyCode: KittyKeyLeft, Modifiers: KittyModShift, EventType: KittyKeyPress},  // DECCKM
	"\x1b[a":    {KeyCode: KittyKeyUp, Modifiers: KittyModShift, EventType: KittyKeyPress},    // urxvt
	"\x1b[b":    {KeyCode: KittyKeyDown, Modifiers: KittyModShift, EventType: KittyKeyPress},  // urxvt
	"\x1b[c":    {KeyCode: KittyKeyRight, Modifiers: KittyModShift, EventType: KittyKeyPress}, // urxvt
	"\x1b[d":    {KeyCode: KittyKeyLeft, Modifiers: KittyModShift, EventType: KittyKeyPress},  // urxvt
	"\x1b[1;3A": {KeyCode: KittyKeyUp, Modifiers: KittyModAlt, EventType: KittyKeyPress},
	"\x1b[1;3B": {KeyCode: KittyKeyDown, Modifiers: KittyModAlt, EventType: KittyKeyPress},
	"\x1b[1;3C": {KeyCode: KittyKeyRight, Modifiers: KittyModAlt, EventType: KittyKeyPress},
	"\x1b[1;3D": {KeyCode: KittyKeyLeft, Modifiers: KittyModAlt, EventType: KittyKeyPress},

	"\x1b[1;4A": {KeyCode: KittyKeyUp, Modifiers: KittyModShift | KittyModAlt, EventType: KittyKeyPress},
	"\x1b[1;4B": {KeyCode: KittyKeyDown, Modifiers: KittyModShift | KittyModAlt, EventType: KittyKeyPress},
	"\x1b[1;4C": {KeyCode: KittyKeyRight, Modifiers: KittyModShift | KittyModAlt, EventType: KittyKeyPress},
	"\x1b[1;4D": {KeyCode: KittyKeyLeft, Modifiers: KittyModShift | KittyModAlt, EventType: KittyKeyPress},

	"\x1b[1;5A": {KeyCode: KittyKeyUp, Modifiers: KittyModCtrl, EventType: KittyKeyPress},
	"\x1b[1;5B": {KeyCode: KittyKeyDown, Modifiers: KittyModCtrl, EventType: KittyKeyPress},
	"\x1b[1;5C": {KeyCode: KittyKeyRight, Modifiers: KittyModCtrl, EventType: KittyKeyPress},
	"\x1b[1;5D": {KeyCode: KittyKeyLeft, Modifiers: KittyModCtrl, EventType: KittyKeyPress},
	"\x1b[Oa":   {KeyCode: KittyKeyUp, Modifiers: KittyModCtrl | KittyModAlt, EventType: KittyKeyPress},    // urxvt
	"\x1b[Ob":   {KeyCode: KittyKeyDown, Modifiers: KittyModCtrl | KittyModAlt, EventType: KittyKeyPress},  // urxvt
	"\x1b[Oc":   {KeyCode: KittyKeyRight, Modifiers: KittyModCtrl | KittyModAlt, EventType: KittyKeyPress}, // urxvt
	"\x1b[Od":   {KeyCode: KittyKeyLeft, Modifiers: KittyModCtrl | KittyModAlt, EventType: KittyKeyPress},  // urxvt
	"\x1b[1;6A": {KeyCode: KittyKeyUp, Modifiers: KittyModCtrl | KittyModShift, EventType: KittyKeyPress},
	"\x1b[1;6B": {KeyCode: KittyKeyDown, Modifiers: KittyModCtrl | KittyModShift, EventType: KittyKeyPress},
	"\x1b[1;6C": {KeyCode: KittyKeyRight, Modifiers: KittyModCtrl | KittyModShift, EventType: KittyKeyPress},
	"\x1b[1;6D": {KeyCode: KittyKeyLeft, Modifiers: KittyModCtrl | KittyModShift, EventType: KittyKeyPress},
	"\x1b[1;7A": {KeyCode: KittyKeyUp, Modifiers: KittyModCtrl | KittyModAlt, EventType: KittyKeyPress},
	"\x1b[1;7B": {KeyCode: KittyKeyDown, Modifiers: KittyModCtrl | KittyModAlt, EventType: KittyKeyPress},
	"\x1b[1;7C": {KeyCode: KittyKeyRight, Modifiers: KittyModCtrl | KittyModAlt, EventType: KittyKeyPress},
	"\x1b[1;7D": {KeyCode: KittyKeyLeft, Modifiers: KittyModCtrl | KittyModAlt, EventType: KittyKeyPress},
	"\x1b[1;8A": {KeyCode: KittyKeyUp, Modifiers: KittyModCtrl | KittyModShift | KittyModAlt, EventType: KittyKeyPress},
	"\x1b[1;8B": {KeyCode: KittyKeyDown, Modifiers: KittyModCtrl | KittyModShift | KittyModAlt, EventType: KittyKeyPress},
	"\x1b[1;8C": {KeyCode: KittyKeyRight, Modifiers: KittyModCtrl | KittyModShift | KittyModAlt, EventType: KittyKeyPress},
	"\x1b[1;8D": {KeyCode: KittyKeyLeft, Modifiers: KittyModCtrl | KittyModShift | KittyModAlt, EventType: KittyKeyPress},

	// Miscellaneous keys
	"\x1b[Z": {KeyCode: KittyKeyTab, Modifiers: KittyModShift, EventType: KittyKeyPress},

	"\x1b[2~":   {KeyCode: KittyKeyInsert, Modifiers: 0, EventType: KittyKeyPress},
	"\x1b[3;2~": {KeyCode: KittyKeyInsert, Modifiers: KittyModAlt, EventType: KittyKeyPress},

	"\x1b[3~":   {KeyCode: KittyKeyDelete, Modifiers: 0, EventType: KittyKeyPress},
	"\x1b[3;3~": {KeyCode: KittyKeyDelete, Modifiers: KittyModAlt, EventType: KittyKeyPress},

	"\x1b[5~":   {KeyCode: KittyKeyPageUp, Modifiers: 0, EventType: KittyKeyPress},
	"\x1b[5;3~": {KeyCode: KittyKeyPageUp, Modifiers: KittyModAlt, EventType: KittyKeyPress},
	"\x1b[5;5~": {KeyCode: KittyKeyPageUp, Modifiers: KittyModCtrl, EventType: KittyKeyPress},
	"\x1b[5^":   {KeyCode: KittyKeyPageUp, Modifiers: KittyModCtrl, EventType: KittyKeyPress}, // urxvt
	"\x1b[5;7~": {KeyCode: KittyKeyPageUp, Modifiers: KittyModCtrl | KittyModAlt, EventType: KittyKeyPress},

	"\x1b[6~":   {KeyCode: KittyKeyPageDown, Modifiers: 0, EventType: KittyKeyPress},
	"\x1b[6;3~": {KeyCode: KittyKeyPageDown, Modifiers: KittyModAlt, EventType: KittyKeyPress},
	"\x1b[6;5~": {KeyCode: KittyKeyPageDown, Modifiers: KittyModCtrl, EventType: KittyKeyPress},
	"\x1b[6^":   {KeyCode: KittyKeyPageDown, Modifiers: KittyModCtrl, EventType: KittyKeyPress}, // urxvt
	"\x1b[6;7~": {KeyCode: KittyKeyPageDown, Modifiers: KittyModCtrl | KittyModAlt, EventType: KittyKeyPress},

	"\x1b[1~":   {KeyCode: KittyKeyHome, Modifiers: 0, EventType: KittyKeyPress},
	"\x1b[H":    {KeyCode: KittyKeyHome, Modifiers: 0, EventType: KittyKeyPress},                     // xterm, lxterm
	"\x1b[1;3H": {KeyCode: KittyKeyHome, Modifiers: KittyModAlt, EventType: KittyKeyPress},          // xterm, lxterm
	"\x1b[1;5H": {KeyCode: KittyKeyHome, Modifiers: KittyModCtrl, EventType: KittyKeyPress},                 // xterm, lxterm
	"\x1b[1;7H": {KeyCode: KittyKeyHome, Modifiers: KittyModCtrl | KittyModAlt, EventType: KittyKeyPress},      // xterm, lxterm
	"\x1b[1;2H": {KeyCode: KittyKeyHome, Modifiers: KittyModShift, EventType: KittyKeyPress},                // xterm, lxterm
	"\x1b[1;4H": {KeyCode: KittyKeyHome, Modifiers: KittyModShift | KittyModAlt, EventType: KittyKeyPress},     // xterm, lxterm
	"\x1b[1;6H": {KeyCode: KittyKeyHome, Modifiers: KittyModCtrl | KittyModShift, EventType: KittyKeyPress},            // xterm, lxterm
	"\x1b[1;8H": {KeyCode: KittyKeyHome, Modifiers: KittyModCtrl | KittyModShift | KittyModAlt, EventType: KittyKeyPress}, // xterm, lxterm

	"\x1b[4~":   {KeyCode: KittyKeyEnd, Modifiers: 0, EventType: KittyKeyPress},
	"\x1b[F":    {KeyCode: KittyKeyEnd, Modifiers: 0, EventType: KittyKeyPress},                     // xterm, lxterm
	"\x1b[1;3F": {KeyCode: KittyKeyEnd, Modifiers: KittyModAlt, EventType: KittyKeyPress},          // xterm, lxterm
	"\x1b[1;5F": {KeyCode: KittyKeyEnd, Modifiers: KittyModCtrl, EventType: KittyKeyPress},                 // xterm, lxterm
	"\x1b[1;7F": {KeyCode: KittyKeyEnd, Modifiers: KittyModCtrl | KittyModAlt, EventType: KittyKeyPress},      // xterm, lxterm
	"\x1b[1;2F": {KeyCode: KittyKeyEnd, Modifiers: KittyModShift, EventType: KittyKeyPress},                // xterm, lxterm
	"\x1b[1;4F": {KeyCode: KittyKeyEnd, Modifiers: KittyModShift | KittyModAlt, EventType: KittyKeyPress},     // xterm, lxterm
	"\x1b[1;6F": {KeyCode: KittyKeyEnd, Modifiers: KittyModCtrl | KittyModShift, EventType: KittyKeyPress},            // xterm, lxterm
	"\x1b[1;8F": {KeyCode: KittyKeyEnd, Modifiers: KittyModCtrl | KittyModShift | KittyModAlt, EventType: KittyKeyPress}, // xterm, lxterm

	"\x1b[7~": {KeyCode: KittyKeyHome, Modifiers: 0, EventType: KittyKeyPress},          // urxvt
	"\x1b[7^": {KeyCode: KittyKeyHome, Modifiers: KittyModCtrl, EventType: KittyKeyPress},      // urxvt
	"\x1b[7$": {KeyCode: KittyKeyHome, Modifiers: KittyModShift, EventType: KittyKeyPress},     // urxvt
	"\x1b[7@": {KeyCode: KittyKeyHome, Modifiers: KittyModCtrl | KittyModShift, EventType: KittyKeyPress}, // urxvt

	"\x1b[8~": {KeyCode: KittyKeyEnd, Modifiers: 0, EventType: KittyKeyPress},          // urxvt
	"\x1b[8^": {KeyCode: KittyKeyEnd, Modifiers: KittyModCtrl, EventType: KittyKeyPress},      // urxvt
	"\x1b[8$": {KeyCode: KittyKeyEnd, Modifiers: KittyModShift, EventType: KittyKeyPress},     // urxvt
	"\x1b[8@": {KeyCode: KittyKeyEnd, Modifiers: KittyModCtrl | KittyModShift, EventType: KittyKeyPress}, // urxvt

	// Function keys, Linux console
	"\x1b[[A": {KeyCode: KittyKeyF1, Modifiers: 0, EventType: KittyKeyPress}, // linux console
	"\x1b[[B": {KeyCode: KittyKeyF2, Modifiers: 0, EventType: KittyKeyPress}, // linux console
	"\x1b[[C": {KeyCode: KittyKeyF3, Modifiers: 0, EventType: KittyKeyPress}, // linux console
	"\x1b[[D": {KeyCode: KittyKeyF4, Modifiers: 0, EventType: KittyKeyPress}, // linux console
	"\x1b[[E": {KeyCode: KittyKeyF5, Modifiers: 0, EventType: KittyKeyPress}, // linux console

	// Function keys, X11
	"\x1bOP": {KeyCode: KittyKeyF1, Modifiers: 0, EventType: KittyKeyPress}, // vt100, xterm
	"\x1bOQ": {KeyCode: KittyKeyF2, Modifiers: 0, EventType: KittyKeyPress}, // vt100, xterm
	"\x1bOR": {KeyCode: KittyKeyF3, Modifiers: 0, EventType: KittyKeyPress}, // vt100, xterm
	"\x1bOS": {KeyCode: KittyKeyF4, Modifiers: 0, EventType: KittyKeyPress}, // vt100, xterm

	"\x1b[1;3P": {KeyCode: KittyKeyF1, Modifiers: KittyModAlt, EventType: KittyKeyPress}, // vt100, xterm
	"\x1b[1;3Q": {KeyCode: KittyKeyF2, Modifiers: KittyModAlt, EventType: KittyKeyPress}, // vt100, xterm
	"\x1b[1;3R": {KeyCode: KittyKeyF3, Modifiers: KittyModAlt, EventType: KittyKeyPress}, // vt100, xterm
	"\x1b[1;3S": {KeyCode: KittyKeyF4, Modifiers: KittyModAlt, EventType: KittyKeyPress}, // vt100, xterm

	"\x1b[11~": {KeyCode: KittyKeyF1, Modifiers: 0, EventType: KittyKeyPress}, // urxvt
	"\x1b[12~": {KeyCode: KittyKeyF2, Modifiers: 0, EventType: KittyKeyPress}, // urxvt
	"\x1b[13~": {KeyCode: KittyKeyF3, Modifiers: 0, EventType: KittyKeyPress}, // urxvt
	"\x1b[14~": {KeyCode: KittyKeyF4, Modifiers: 0, EventType: KittyKeyPress}, // urxvt

	"\x1b[15~": {KeyCode: KittyKeyF5, Modifiers: 0, EventType: KittyKeyPress}, // vt100, xterm, also urxvt

	"\x1b[15;3~": {KeyCode: KittyKeyF5, Modifiers: KittyModAlt, EventType: KittyKeyPress}, // vt100, xterm, also urxvt

	"\x1b[17~": {KeyCode: KittyKeyF6, Modifiers: 0, EventType: KittyKeyPress},  // vt100, xterm, also urxvt
	"\x1b[18~": {KeyCode: KittyKeyF7, Modifiers: 0, EventType: KittyKeyPress},  // vt100, xterm, also urxvt
	"\x1b[19~": {KeyCode: KittyKeyF8, Modifiers: 0, EventType: KittyKeyPress},  // vt100, xterm, also urxvt
	"\x1b[20~": {KeyCode: KittyKeyF9, Modifiers: 0, EventType: KittyKeyPress},  // vt100, xterm, also urxvt
	"\x1b[21~": {KeyCode: KittyKeyF10, Modifiers: 0, EventType: KittyKeyPress}, // vt100, xterm, also urxvt

	"\x1b[17;3~": {KeyCode: KittyKeyF6, Modifiers: KittyModAlt, EventType: KittyKeyPress},  // vt100, xterm
	"\x1b[18;3~": {KeyCode: KittyKeyF7, Modifiers: KittyModAlt, EventType: KittyKeyPress},  // vt100, xterm
	"\x1b[19;3~": {KeyCode: KittyKeyF8, Modifiers: KittyModAlt, EventType: KittyKeyPress},  // vt100, xterm
	"\x1b[20;3~": {KeyCode: KittyKeyF9, Modifiers: KittyModAlt, EventType: KittyKeyPress},  // vt100, xterm
	"\x1b[21;3~": {KeyCode: KittyKeyF10, Modifiers: KittyModAlt, EventType: KittyKeyPress}, // vt100, xterm

	"\x1b[23~": {KeyCode: KittyKeyF11, Modifiers: 0, EventType: KittyKeyPress}, // vt100, xterm, also urxvt
	"\x1b[24~": {KeyCode: KittyKeyF12, Modifiers: 0, EventType: KittyKeyPress}, // vt100, xterm, also urxvt

	"\x1b[23;3~": {KeyCode: KittyKeyF11, Modifiers: KittyModAlt, EventType: KittyKeyPress}, // vt100, xterm
	"\x1b[24;3~": {KeyCode: KittyKeyF12, Modifiers: KittyModAlt, EventType: KittyKeyPress}, // vt100, xterm

	"\x1b[1;2P": {KeyCode: 0xE000 + 124, Modifiers: 0, EventType: KittyKeyPress}, // F13
	"\x1b[1;2Q": {KeyCode: 0xE000 + 125, Modifiers: 0, EventType: KittyKeyPress}, // F14

	"\x1b[25~": {KeyCode: 0xE000 + 124, Modifiers: 0, EventType: KittyKeyPress}, // F13, vt100, xterm, also urxvt
	"\x1b[26~": {KeyCode: 0xE000 + 125, Modifiers: 0, EventType: KittyKeyPress}, // F14, vt100, xterm, also urxvt

	"\x1b[25;3~": {KeyCode: 0xE000 + 124, Modifiers: KittyModAlt, EventType: KittyKeyPress}, // F13, vt100, xterm
	"\x1b[26;3~": {KeyCode: 0xE000 + 125, Modifiers: KittyModAlt, EventType: KittyKeyPress}, // F14, vt100, xterm

	"\x1b[1;2R": {KeyCode: 0xE000 + 126, Modifiers: 0, EventType: KittyKeyPress}, // F15
	"\x1b[1;2S": {KeyCode: 0xE000 + 127, Modifiers: 0, EventType: KittyKeyPress}, // F16

	"\x1b[28~": {KeyCode: 0xE000 + 126, Modifiers: 0, EventType: KittyKeyPress}, // F15, vt100, xterm, also urxvt
	"\x1b[29~": {KeyCode: 0xE000 + 127, Modifiers: 0, EventType: KittyKeyPress}, // F16, vt100, xterm, also urxvt

	"\x1b[28;3~": {KeyCode: 0xE000 + 126, Modifiers: KittyModAlt, EventType: KittyKeyPress}, // F15, vt100, xterm
	"\x1b[29;3~": {KeyCode: 0xE000 + 127, Modifiers: KittyModAlt, EventType: KittyKeyPress}, // F16, vt100, xterm

	"\x1b[15;2~": {KeyCode: 0xE000 + 128, Modifiers: 0, EventType: KittyKeyPress}, // F17
	"\x1b[17;2~": {KeyCode: 0xE000 + 129, Modifiers: 0, EventType: KittyKeyPress}, // F18
	"\x1b[18;2~": {KeyCode: 0xE000 + 130, Modifiers: 0, EventType: KittyKeyPress}, // F19
	"\x1b[19;2~": {KeyCode: 0xE000 + 131, Modifiers: 0, EventType: KittyKeyPress}, // F20

	"\x1b[31~": {KeyCode: 0xE000 + 128, Modifiers: 0, EventType: KittyKeyPress}, // F17
	"\x1b[32~": {KeyCode: 0xE000 + 129, Modifiers: 0, EventType: KittyKeyPress}, // F18
	"\x1b[33~": {KeyCode: 0xE000 + 130, Modifiers: 0, EventType: KittyKeyPress}, // F19
	"\x1b[34~": {KeyCode: 0xE000 + 131, Modifiers: 0, EventType: KittyKeyPress}, // F20

	// Powershell sequences.
	"\x1bOA": {KeyCode: KittyKeyUp, Modifiers: 0, EventType: KittyKeyPress},
	"\x1bOB": {KeyCode: KittyKeyDown, Modifiers: 0, EventType: KittyKeyPress},
	"\x1bOC": {KeyCode: KittyKeyRight, Modifiers: 0, EventType: KittyKeyPress},
	"\x1bOD": {KeyCode: KittyKeyLeft, Modifiers: 0, EventType: KittyKeyPress},
}

// xtermSequences is an authoritative list of the byte sequences for keys to
// be interpreted by xterm, because sequences above contains conflicting keys.
// We probably need a better approach to this.
var xtermSequences = map[string]Key{
	"\x1b[A":    {KeyCode: KittyKeyUp, EventType: KittyKeyPress},
	"\x1b[B":    {KeyCode: KittyKeyDown, EventType: KittyKeyPress},
	"\x1b[C":    {KeyCode: KittyKeyRight, EventType: KittyKeyPress},
	"\x1b[D":    {KeyCode: KittyKeyLeft, EventType: KittyKeyPress},
	"\x1b[1;2A": {KeyCode: KittyKeyUp, Modifiers: KittyModShift, EventType: KittyKeyPress},
	"\x1b[1;2B": {KeyCode: KittyKeyDown, Modifiers: KittyModShift, EventType: KittyKeyPress},
	"\x1b[1;2C": {KeyCode: KittyKeyRight, Modifiers: KittyModShift, EventType: KittyKeyPress},
	"\x1b[1;2D": {KeyCode: KittyKeyLeft, Modifiers: KittyModShift, EventType: KittyKeyPress},

	"\x1b[OA": {KeyCode: KittyKeyUp, Modifiers: KittyModShift, EventType: KittyKeyPress},    // DECCKM
	"\x1b[OB": {KeyCode: KittyKeyDown, Modifiers: KittyModShift, EventType: KittyKeyPress},  // DECCKM
	"\x1b[OC": {KeyCode: KittyKeyRight, Modifiers: KittyModShift, EventType: KittyKeyPress}, // DECCKM
	"\x1b[OD": {KeyCode: KittyKeyLeft, Modifiers: KittyModShift, EventType: KittyKeyPress},  // DECCKM

	"\x1b[1;5A": {KeyCode: KittyKeyUp, Modifiers: KittyModCtrl, EventType: KittyKeyPress},
	"\x1b[1;5B": {KeyCode: KittyKeyDown, Modifiers: KittyModCtrl, EventType: KittyKeyPress},
	"\x1b[1;5C": {KeyCode: KittyKeyRight, Modifiers: KittyModCtrl, EventType: KittyKeyPress},
	"\x1b[1;5D": {KeyCode: KittyKeyLeft, Modifiers: KittyModCtrl, EventType: KittyKeyPress},

	"\x1b[5;5~": {KeyCode: KittyKeyPageUp, Modifiers: KittyModCtrl, EventType: KittyKeyPress},
	"\x1b[6;5~": {KeyCode: KittyKeyPageDown, Modifiers: KittyModCtrl, EventType: KittyKeyPress},

	"\x1b[H":    {KeyCode: KittyKeyHome, EventType: KittyKeyPress},                     // xterm, lxterm
	"\x1b[1;3H": {KeyCode: KittyKeyHome, Modifiers: KittyModAlt, EventType: KittyKeyPress},          // xterm, lxterm
	"\x1b[1;5H": {KeyCode: KittyKeyHome, Modifiers: KittyModCtrl, EventType: KittyKeyPress},                 // xterm, lxterm
	"\x1b[1;7H": {KeyCode: KittyKeyHome, Modifiers: KittyModCtrl | KittyModAlt, EventType: KittyKeyPress},      // xterm, lxterm
	"\x1b[1;2H": {KeyCode: KittyKeyHome, Modifiers: KittyModShift, EventType: KittyKeyPress},                // xterm, lxterm
	"\x1b[1;4H": {KeyCode: KittyKeyHome, Modifiers: KittyModShift | KittyModAlt, EventType: KittyKeyPress},     // xterm, lxterm
	"\x1b[1;6H": {KeyCode: KittyKeyHome, Modifiers: KittyModCtrl | KittyModShift, EventType: KittyKeyPress},            // xterm, lxterm
	"\x1b[1;8H": {KeyCode: KittyKeyHome, Modifiers: KittyModCtrl | KittyModShift | KittyModAlt, EventType: KittyKeyPress}, // xterm, lxterm

	"\x1b[F":    {KeyCode: KittyKeyEnd, EventType: KittyKeyPress},                     // xterm, lxterm
	"\x1b[1;3F": {KeyCode: KittyKeyEnd, Modifiers: KittyModAlt, EventType: KittyKeyPress},          // xterm, lxterm
	"\x1b[1;5F": {KeyCode: KittyKeyEnd, Modifiers: KittyModCtrl, EventType: KittyKeyPress},                 // xterm, lxterm
	"\x1b[1;7F": {KeyCode: KittyKeyEnd, Modifiers: KittyModCtrl | KittyModAlt, EventType: KittyKeyPress},      // xterm, lxterm
	"\x1b[1;2F": {KeyCode: KittyKeyEnd, Modifiers: KittyModShift, EventType: KittyKeyPress},                // xterm, lxterm
	"\x1b[1;4F": {KeyCode: KittyKeyEnd, Modifiers: KittyModShift | KittyModAlt, EventType: KittyKeyPress},     // xterm, lxterm
	"\x1b[1;6F": {KeyCode: KittyKeyEnd, Modifiers: KittyModCtrl | KittyModShift, EventType: KittyKeyPress},            // xterm, lxterm
	"\x1b[1;8F": {KeyCode: KittyKeyEnd, Modifiers: KittyModCtrl | KittyModShift | KittyModAlt, EventType: KittyKeyPress}, // xterm, lxterm

	"\x1bOP": {KeyCode: KittyKeyF1, EventType: KittyKeyPress}, // vt100, xterm
	"\x1bOQ": {KeyCode: KittyKeyF2, EventType: KittyKeyPress}, // vt100, xterm
	"\x1bOR": {KeyCode: KittyKeyF3, EventType: KittyKeyPress}, // vt100, xterm
	"\x1bOS": {KeyCode: KittyKeyF4, EventType: KittyKeyPress}, // vt100, xterm

	"\x1b[1;3P": {KeyCode: KittyKeyF1, Modifiers: KittyModAlt, EventType: KittyKeyPress}, // vt100, xterm
	"\x1b[1;3Q": {KeyCode: KittyKeyF2, Modifiers: KittyModAlt, EventType: KittyKeyPress}, // vt100, xterm
	"\x1b[1;3R": {KeyCode: KittyKeyF3, Modifiers: KittyModAlt, EventType: KittyKeyPress}, // vt100, xterm
	"\x1b[1;3S": {KeyCode: KittyKeyF4, Modifiers: KittyModAlt, EventType: KittyKeyPress}, // vt100, xterm

	"\x1b[28~": {KeyCode: 0xE000 + 126, EventType: KittyKeyPress}, // F15 - vt100, xterm, also urxvt
	"\x1b[29~": {KeyCode: 0xE000 + 127, EventType: KittyKeyPress}, // F16 - vt100, xterm, also urxvt
}

type keyLookup struct {
	Type KeyType
	Alt  bool
}

// inverseSequences is a mapping from a Key to its byte sequence.
var inverseSequences = func() map[keyLookup][]byte {
	s := map[keyLookup][]byte{}
	for str, key := range extSequences {
		legacyType := key.toLegacyKeyType()
		s[keyLookup{
			Type: legacyType,
			Alt:  key.Modifiers&KittyModAlt != 0,
		}] = []byte(str)
	}

	for str, key := range xtermSequences {
		legacyType := key.toLegacyKeyType()
		s[keyLookup{
			Type: legacyType,
			Alt:  key.Modifiers&KittyModAlt != 0,
		}] = []byte(str)
	}
	return s
}()
