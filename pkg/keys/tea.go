package keys

import (
	tea "github.com/charmbracelet/bubbletea"
)

// toLegacyKeyType converts a Kitty key code to legacy KeyType
func (k Key) toLegacyKeyType() tea.KeyType {
	if len(k.Runes) == 0 {
		return 0
	}

	keyCode := k.Runes[0]
	switch keyCode {
	case KittyKeyEscape:
		return KeyEscape
	case KittyKeyEnter:
		return KeyEnter
	case KittyKeyTab:
		return KeyTab
	case KittyKeyBackspace:
		return KeyBackspace
	case KittyKeyInsert:
		return KeyInsert
	case KittyKeyDelete:
		return KeyDelete
	case KittyKeyHome:
		if k.Mod&KeyModCtrl != 0 && k.Mod&KeyModShift != 0 {
			return KeyCtrlShiftHome
		} else if k.Mod&KeyModCtrl != 0 {
			return KeyCtrlHome
		} else if k.Mod&KeyModShift != 0 {
			return KeyShiftHome
		} else {
			return KeyHome
		}
	case KittyKeyEnd:
		if k.Mod&KeyModCtrl != 0 && k.Mod&KeyModShift != 0 {
			return KeyCtrlShiftEnd
		} else if k.Mod&KeyModCtrl != 0 {
			return KeyCtrlEnd
		} else if k.Mod&KeyModShift != 0 {
			return KeyShiftEnd
		} else {
			return KeyEnd
		}
	case KittyKeyPageUp:
		if k.Mod&KeyModCtrl != 0 {
			return KeyCtrlPgUp
		} else {
			return KeyPgUp
		}
	case KittyKeyPageDown:
		if k.Mod&KeyModCtrl != 0 {
			return KeyCtrlPgDown
		} else {
			return KeyPgDown
		}
	case KittyKeyLeft:
		if k.Mod&KeyModCtrl != 0 && k.Mod&KeyModShift != 0 {
			return KeyCtrlShiftLeft
		} else if k.Mod&KeyModCtrl != 0 {
			return KeyCtrlLeft
		} else if k.Mod&KeyModShift != 0 {
			return KeyShiftLeft
		} else {
			return KeyLeft
		}
	case KittyKeyUp:
		if k.Mod&KeyModCtrl != 0 && k.Mod&KeyModShift != 0 {
			return KeyCtrlShiftUp
		} else if k.Mod&KeyModCtrl != 0 {
			return KeyCtrlUp
		} else if k.Mod&KeyModShift != 0 {
			return KeyShiftUp
		} else {
			return KeyUp
		}
	case KittyKeyRight:
		if k.Mod&KeyModCtrl != 0 && k.Mod&KeyModShift != 0 {
			return KeyCtrlShiftRight
		} else if k.Mod&KeyModCtrl != 0 {
			return KeyCtrlRight
		} else if k.Mod&KeyModShift != 0 {
			return KeyShiftRight
		} else {
			return KeyRight
		}
	case KittyKeyDown:
		if k.Mod&KeyModCtrl != 0 && k.Mod&KeyModShift != 0 {
			return KeyCtrlShiftDown
		} else if k.Mod&KeyModCtrl != 0 {
			return KeyCtrlDown
		} else if k.Mod&KeyModShift != 0 {
			return KeyShiftDown
		} else {
			return KeyDown
		}
	case KittyKeyF1:
		return KeyF1
	case KittyKeyF2:
		return KeyF2
	case KittyKeyF3:
		return KeyF3
	case KittyKeyF4:
		return KeyF4
	case KittyKeyF5:
		return KeyF5
	case KittyKeyF6:
		return KeyF6
	case KittyKeyF7:
		return KeyF7
	case KittyKeyF8:
		return KeyF8
	case KittyKeyF9:
		return KeyF9
	case KittyKeyF10:
		return KeyF10
	case KittyKeyF11:
		return KeyF11
	case KittyKeyF12:
		return KeyF12
	default:
		return 0
	}
}
