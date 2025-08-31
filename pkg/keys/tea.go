package keys

import (
	tea "github.com/charmbracelet/bubbletea"
)

var teaKeyLookup = map[keyLookup]tea.KeyType{
	k(KittyKeyEscape).toLookup():    tea.KeyEscape,
	k(KittyKeyEnter).toLookup():     tea.KeyEnter,
	k(KittyKeyTab).toLookup():       tea.KeyTab,
	k(KittyKeyBackspace).toLookup(): tea.KeyBackspace,
	k(KittyKeyInsert).toLookup():    tea.KeyInsert,
	k(KittyKeyDelete).toLookup():    tea.KeyDelete,

	k(KittyKeyHome).toLookup():                            tea.KeyHome,
	kMod(KittyKeyHome, KeyModShift).toLookup():            tea.KeyShiftHome,
	kMod(KittyKeyHome, KeyModCtrl).toLookup():             tea.KeyCtrlHome,
	kMod(KittyKeyHome, KeyModCtrl|KeyModShift).toLookup(): tea.KeyCtrlShiftHome,

	k(KittyKeyEnd).toLookup():                            tea.KeyEnd,
	kMod(KittyKeyEnd, KeyModShift).toLookup():            tea.KeyShiftEnd,
	kMod(KittyKeyEnd, KeyModCtrl).toLookup():             tea.KeyCtrlEnd,
	kMod(KittyKeyEnd, KeyModCtrl|KeyModShift).toLookup(): tea.KeyCtrlShiftEnd,

	k(KittyKeyPageUp).toLookup():                tea.KeyPgUp,
	kMod(KittyKeyPageUp, KeyModCtrl).toLookup(): tea.KeyCtrlPgUp,

	k(KittyKeyPageDown).toLookup():                tea.KeyPgDown,
	kMod(KittyKeyPageDown, KeyModCtrl).toLookup(): tea.KeyCtrlPgDown,

	k(KittyKeyLeft).toLookup():                            tea.KeyLeft,
	kMod(KittyKeyLeft, KeyModShift).toLookup():            tea.KeyShiftLeft,
	kMod(KittyKeyLeft, KeyModCtrl).toLookup():             tea.KeyCtrlLeft,
	kMod(KittyKeyLeft, KeyModCtrl|KeyModShift).toLookup(): tea.KeyCtrlShiftLeft,

	k(KittyKeyUp).toLookup():                            tea.KeyUp,
	kMod(KittyKeyUp, KeyModShift).toLookup():            tea.KeyShiftUp,
	kMod(KittyKeyUp, KeyModCtrl).toLookup():             tea.KeyCtrlUp,
	kMod(KittyKeyUp, KeyModCtrl|KeyModShift).toLookup(): tea.KeyCtrlShiftUp,

	k(KittyKeyRight).toLookup():                            tea.KeyRight,
	kMod(KittyKeyRight, KeyModShift).toLookup():            tea.KeyShiftRight,
	kMod(KittyKeyRight, KeyModCtrl).toLookup():             tea.KeyCtrlRight,
	kMod(KittyKeyRight, KeyModCtrl|KeyModShift).toLookup(): tea.KeyCtrlShiftRight,

	k(KittyKeyDown).toLookup():                            tea.KeyDown,
	kMod(KittyKeyDown, KeyModShift).toLookup():            tea.KeyShiftDown,
	kMod(KittyKeyDown, KeyModCtrl).toLookup():             tea.KeyCtrlDown,
	kMod(KittyKeyDown, KeyModCtrl|KeyModShift).toLookup(): tea.KeyCtrlShiftDown,

	k(KittyKeyF1).toLookup():  tea.KeyF1,
	k(KittyKeyF2).toLookup():  tea.KeyF2,
	k(KittyKeyF3).toLookup():  tea.KeyF3,
	k(KittyKeyF4).toLookup():  tea.KeyF4,
	k(KittyKeyF5).toLookup():  tea.KeyF5,
	k(KittyKeyF6).toLookup():  tea.KeyF6,
	k(KittyKeyF7).toLookup():  tea.KeyF7,
	k(KittyKeyF8).toLookup():  tea.KeyF8,
	k(KittyKeyF9).toLookup():  tea.KeyF9,
	k(KittyKeyF10).toLookup(): tea.KeyF10,
	k(KittyKeyF11).toLookup(): tea.KeyF11,
	k(KittyKeyF12).toLookup(): tea.KeyF12,

	// Control keys
	kMod('@', KeyModCtrl).toLookup():  tea.KeyCtrlAt,
	kMod('a', KeyModCtrl).toLookup():  tea.KeyCtrlA,
	kMod('b', KeyModCtrl).toLookup():  tea.KeyCtrlB,
	kMod('c', KeyModCtrl).toLookup():  tea.KeyCtrlC,
	kMod('d', KeyModCtrl).toLookup():  tea.KeyCtrlD,
	kMod('e', KeyModCtrl).toLookup():  tea.KeyCtrlE,
	kMod('f', KeyModCtrl).toLookup():  tea.KeyCtrlF,
	kMod('g', KeyModCtrl).toLookup():  tea.KeyCtrlG,
	kMod('h', KeyModCtrl).toLookup():  tea.KeyCtrlH,
	kMod('i', KeyModCtrl).toLookup():  tea.KeyCtrlI,
	kMod('j', KeyModCtrl).toLookup():  tea.KeyCtrlJ,
	kMod('k', KeyModCtrl).toLookup():  tea.KeyCtrlK,
	kMod('l', KeyModCtrl).toLookup():  tea.KeyCtrlL,
	kMod('m', KeyModCtrl).toLookup():  tea.KeyCtrlM,
	kMod('n', KeyModCtrl).toLookup():  tea.KeyCtrlN,
	kMod('o', KeyModCtrl).toLookup():  tea.KeyCtrlO,
	kMod('p', KeyModCtrl).toLookup():  tea.KeyCtrlP,
	kMod('q', KeyModCtrl).toLookup():  tea.KeyCtrlQ,
	kMod('r', KeyModCtrl).toLookup():  tea.KeyCtrlR,
	kMod('s', KeyModCtrl).toLookup():  tea.KeyCtrlS,
	kMod('t', KeyModCtrl).toLookup():  tea.KeyCtrlT,
	kMod('u', KeyModCtrl).toLookup():  tea.KeyCtrlU,
	kMod('v', KeyModCtrl).toLookup():  tea.KeyCtrlV,
	kMod('w', KeyModCtrl).toLookup():  tea.KeyCtrlW,
	kMod('x', KeyModCtrl).toLookup():  tea.KeyCtrlX,
	kMod('y', KeyModCtrl).toLookup():  tea.KeyCtrlY,
	kMod('z', KeyModCtrl).toLookup():  tea.KeyCtrlZ,
	kMod('[', KeyModCtrl).toLookup():  tea.KeyCtrlOpenBracket,
	kMod('\\', KeyModCtrl).toLookup(): tea.KeyCtrlBackslash,
	kMod(']', KeyModCtrl).toLookup():  tea.KeyCtrlCloseBracket,
	kMod('^', KeyModCtrl).toLookup():  tea.KeyCtrlCaret,
	kMod('_', KeyModCtrl).toLookup():  tea.KeyCtrlUnderscore,
	kMod('?', KeyModCtrl).toLookup():  tea.KeyCtrlQuestionMark,

	// Additional keys
	k(' ').toLookup():                         tea.KeySpace,
	kMod(KittyKeyTab, KeyModShift).toLookup(): tea.KeyShiftTab,
}

func (k Key) Tea() (msg tea.KeyMsg, ok bool) {
	// bubbletea does not handle release/repeat events
	if k.Type != KeyEventPress {
		return
	}

	if keyType, ok := teaKeyLookup[k.toLookup()]; ok {
		return tea.KeyMsg{Type: keyType}, true
	}

	return tea.KeyMsg{
		Type:  tea.KeyRunes,
		Runes: onlyPrintable(k.Runes),
		Alt:   k.Mod&KeyModAlt != 0,
	}, true
}
