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
		return tea.KeyEscape
	case KittyKeyEnter:
		return tea.KeyEnter
	case KittyKeyTab:
		return tea.KeyTab
	case KittyKeyBackspace:
		return tea.KeyBackspace
	case KittyKeyInsert:
		return tea.KeyInsert
	case KittyKeyDelete:
		return tea.KeyDelete
	case KittyKeyHome:
		if k.Mod&KeyModCtrl != 0 && k.Mod&KeyModShift != 0 {
			return tea.KeyCtrlShiftHome
		} else if k.Mod&KeyModCtrl != 0 {
			return tea.KeyCtrlHome
		} else if k.Mod&KeyModShift != 0 {
			return tea.KeyShiftHome
		} else {
			return tea.KeyHome
		}
	case KittyKeyEnd:
		if k.Mod&KeyModCtrl != 0 && k.Mod&KeyModShift != 0 {
			return tea.KeyCtrlShiftEnd
		} else if k.Mod&KeyModCtrl != 0 {
			return tea.KeyCtrlEnd
		} else if k.Mod&KeyModShift != 0 {
			return tea.KeyShiftEnd
		} else {
			return tea.KeyEnd
		}
	case KittyKeyPageUp:
		if k.Mod&KeyModCtrl != 0 {
			return tea.KeyCtrlPgUp
		} else {
			return tea.KeyPgUp
		}
	case KittyKeyPageDown:
		if k.Mod&KeyModCtrl != 0 {
			return tea.KeyCtrlPgDown
		} else {
			return tea.KeyPgDown
		}
	case KittyKeyLeft:
		if k.Mod&KeyModCtrl != 0 && k.Mod&KeyModShift != 0 {
			return tea.KeyCtrlShiftLeft
		} else if k.Mod&KeyModCtrl != 0 {
			return tea.KeyCtrlLeft
		} else if k.Mod&KeyModShift != 0 {
			return tea.KeyShiftLeft
		} else {
			return tea.KeyLeft
		}
	case KittyKeyUp:
		if k.Mod&KeyModCtrl != 0 && k.Mod&KeyModShift != 0 {
			return tea.KeyCtrlShiftUp
		} else if k.Mod&KeyModCtrl != 0 {
			return tea.KeyCtrlUp
		} else if k.Mod&KeyModShift != 0 {
			return tea.KeyShiftUp
		} else {
			return tea.KeyUp
		}
	case KittyKeyRight:
		if k.Mod&KeyModCtrl != 0 && k.Mod&KeyModShift != 0 {
			return tea.KeyCtrlShiftRight
		} else if k.Mod&KeyModCtrl != 0 {
			return tea.KeyCtrlRight
		} else if k.Mod&KeyModShift != 0 {
			return tea.KeyShiftRight
		} else {
			return tea.KeyRight
		}
	case KittyKeyDown:
		if k.Mod&KeyModCtrl != 0 && k.Mod&KeyModShift != 0 {
			return tea.KeyCtrlShiftDown
		} else if k.Mod&KeyModCtrl != 0 {
			return tea.KeyCtrlDown
		} else if k.Mod&KeyModShift != 0 {
			return tea.KeyShiftDown
		} else {
			return tea.KeyDown
		}
	case KittyKeyF1:
		return tea.KeyF1
	case KittyKeyF2:
		return tea.KeyF2
	case KittyKeyF3:
		return tea.KeyF3
	case KittyKeyF4:
		return tea.KeyF4
	case KittyKeyF5:
		return tea.KeyF5
	case KittyKeyF6:
		return tea.KeyF6
	case KittyKeyF7:
		return tea.KeyF7
	case KittyKeyF8:
		return tea.KeyF8
	case KittyKeyF9:
		return tea.KeyF9
	case KittyKeyF10:
		return tea.KeyF10
	case KittyKeyF11:
		return tea.KeyF11
	case KittyKeyF12:
		return tea.KeyF12
	default:
		return 0
	}
}
