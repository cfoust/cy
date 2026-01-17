package keys

import (
	"github.com/cfoust/cy/pkg/emu"

	tea "github.com/charmbracelet/bubbletea"
)

var teaKeyLookup = map[Key]tea.KeyType{
	k(KittyKeyEscape):    tea.KeyEscape,
	k(KittyKeyEnter):     tea.KeyEnter,
	k(KittyKeyTab):       tea.KeyTab,
	k(KittyKeyBackspace): tea.KeyBackspace,
	k(KittyKeyInsert):    tea.KeyInsert,
	k(KittyKeyDelete):    tea.KeyDelete,

	k(KittyKeyHome):                            tea.KeyHome,
	kMod(KittyKeyHome, KeyModShift):            tea.KeyShiftHome,
	kMod(KittyKeyHome, KeyModCtrl):             tea.KeyCtrlHome,
	kMod(KittyKeyHome, KeyModCtrl|KeyModShift): tea.KeyCtrlShiftHome,

	k(KittyKeyEnd):                            tea.KeyEnd,
	kMod(KittyKeyEnd, KeyModShift):            tea.KeyShiftEnd,
	kMod(KittyKeyEnd, KeyModCtrl):             tea.KeyCtrlEnd,
	kMod(KittyKeyEnd, KeyModCtrl|KeyModShift): tea.KeyCtrlShiftEnd,

	k(KittyKeyPageUp):                tea.KeyPgUp,
	kMod(KittyKeyPageUp, KeyModCtrl): tea.KeyCtrlPgUp,

	k(KittyKeyPageDown):                tea.KeyPgDown,
	kMod(KittyKeyPageDown, KeyModCtrl): tea.KeyCtrlPgDown,

	k(KittyKeyLeft):                            tea.KeyLeft,
	kMod(KittyKeyLeft, KeyModShift):            tea.KeyShiftLeft,
	kMod(KittyKeyLeft, KeyModCtrl):             tea.KeyCtrlLeft,
	kMod(KittyKeyLeft, KeyModCtrl|KeyModShift): tea.KeyCtrlShiftLeft,

	k(KittyKeyUp):                            tea.KeyUp,
	kMod(KittyKeyUp, KeyModShift):            tea.KeyShiftUp,
	kMod(KittyKeyUp, KeyModCtrl):             tea.KeyCtrlUp,
	kMod(KittyKeyUp, KeyModCtrl|KeyModShift): tea.KeyCtrlShiftUp,

	k(KittyKeyRight):                            tea.KeyRight,
	kMod(KittyKeyRight, KeyModShift):            tea.KeyShiftRight,
	kMod(KittyKeyRight, KeyModCtrl):             tea.KeyCtrlRight,
	kMod(KittyKeyRight, KeyModCtrl|KeyModShift): tea.KeyCtrlShiftRight,

	k(KittyKeyDown):                            tea.KeyDown,
	kMod(KittyKeyDown, KeyModShift):            tea.KeyShiftDown,
	kMod(KittyKeyDown, KeyModCtrl):             tea.KeyCtrlDown,
	kMod(KittyKeyDown, KeyModCtrl|KeyModShift): tea.KeyCtrlShiftDown,

	k(KittyKeyF1):  tea.KeyF1,
	k(KittyKeyF2):  tea.KeyF2,
	k(KittyKeyF3):  tea.KeyF3,
	k(KittyKeyF4):  tea.KeyF4,
	k(KittyKeyF5):  tea.KeyF5,
	k(KittyKeyF6):  tea.KeyF6,
	k(KittyKeyF7):  tea.KeyF7,
	k(KittyKeyF8):  tea.KeyF8,
	k(KittyKeyF9):  tea.KeyF9,
	k(KittyKeyF10): tea.KeyF10,
	k(KittyKeyF11): tea.KeyF11,
	k(KittyKeyF12): tea.KeyF12,

	// Control keys
	kMod('@', KeyModCtrl):  tea.KeyCtrlAt,
	kMod('a', KeyModCtrl):  tea.KeyCtrlA,
	kMod('b', KeyModCtrl):  tea.KeyCtrlB,
	kMod('c', KeyModCtrl):  tea.KeyCtrlC,
	kMod('d', KeyModCtrl):  tea.KeyCtrlD,
	kMod('e', KeyModCtrl):  tea.KeyCtrlE,
	kMod('f', KeyModCtrl):  tea.KeyCtrlF,
	kMod('g', KeyModCtrl):  tea.KeyCtrlG,
	kMod('h', KeyModCtrl):  tea.KeyCtrlH,
	kMod('i', KeyModCtrl):  tea.KeyCtrlI,
	kMod('j', KeyModCtrl):  tea.KeyCtrlJ,
	kMod('k', KeyModCtrl):  tea.KeyCtrlK,
	kMod('l', KeyModCtrl):  tea.KeyCtrlL,
	kMod('m', KeyModCtrl):  tea.KeyCtrlM,
	kMod('n', KeyModCtrl):  tea.KeyCtrlN,
	kMod('o', KeyModCtrl):  tea.KeyCtrlO,
	kMod('p', KeyModCtrl):  tea.KeyCtrlP,
	kMod('q', KeyModCtrl):  tea.KeyCtrlQ,
	kMod('r', KeyModCtrl):  tea.KeyCtrlR,
	kMod('s', KeyModCtrl):  tea.KeyCtrlS,
	kMod('t', KeyModCtrl):  tea.KeyCtrlT,
	kMod('u', KeyModCtrl):  tea.KeyCtrlU,
	kMod('v', KeyModCtrl):  tea.KeyCtrlV,
	kMod('w', KeyModCtrl):  tea.KeyCtrlW,
	kMod('x', KeyModCtrl):  tea.KeyCtrlX,
	kMod('y', KeyModCtrl):  tea.KeyCtrlY,
	kMod('z', KeyModCtrl):  tea.KeyCtrlZ,
	kMod('[', KeyModCtrl):  tea.KeyCtrlOpenBracket,
	kMod('\\', KeyModCtrl): tea.KeyCtrlBackslash,
	kMod(']', KeyModCtrl):  tea.KeyCtrlCloseBracket,
	kMod('^', KeyModCtrl):  tea.KeyCtrlCaret,
	kMod('_', KeyModCtrl):  tea.KeyCtrlUnderscore,
	kMod('?', KeyModCtrl):  tea.KeyCtrlQuestionMark,

	// Additional keys
	kMod(KittyKeyTab, KeyModShift): tea.KeyShiftTab,
}

func (k Key) Tea() (msg tea.KeyMsg, ok bool) {
	if _, ok = k.Bytes(emu.DefaultMode, emu.KeyLegacy); !ok {
		return
	}

	// Press and repeat should still query the same key
	if keyType, ok := teaKeyLookup[Key{
		Code: k.Code,
		Mod:  k.Mod,
	}]; ok {
		return tea.KeyMsg{Type: keyType}, true
	}

	code := k.Code

	if code == KeyText {
		return tea.KeyMsg{
			Type:  tea.KeyRunes,
			Runes: []rune(k.Text),
			Alt:   k.HasAlt(),
		}, true
	}

	if k.HasShift() {
		code = k.Shifted
	}

	return tea.KeyMsg{
		Type:  tea.KeyRunes,
		Runes: []rune{code},
		Alt:   k.HasAlt(),
	}, true
}
