package emu

import (
	"fmt"

	"github.com/cfoust/cy/pkg/geom"
)

type KeyProtocol int

const (
	KeyLegacy             KeyProtocol = 0
	KeyDisambiguateEscape KeyProtocol = 1 << (iota - 1)
	KeyReportEventTypes
	KeyReportAlternateKeys
	KeyReportAllKeys
	KeyReportAssociatedText
	KeyReportAll = KeyDisambiguateEscape | KeyReportEventTypes | KeyReportAlternateKeys | KeyReportAllKeys | KeyReportAssociatedText
)

func (k KeyProtocol) String() string {
	if k == KeyLegacy {
		return "legacy"
	}

	return fmt.Sprintf(
		"kitty esc=%t event=%t alt=%t all=%t text=%t",
		(k&KeyDisambiguateEscape) > 0,
		(k&KeyReportEventTypes) > 0,
		(k&KeyReportAlternateKeys) > 0,
		(k&KeyReportAllKeys) > 0,
		(k&KeyReportAssociatedText) > 0,
	)
}

type KeyProtocolState struct {
	Flags KeyProtocol
	Stack []KeyProtocol
}

func NewKeyProtocolState() *KeyProtocolState {
	return &KeyProtocolState{
		Flags: 0,
		Stack: make([]KeyProtocol, 0),
	}
}

// IsEnabled returns whether Kitty protocol is currently enabled
func (k *KeyProtocolState) IsEnabled() bool {
	return k.Flags > 0
}

// GetFlags returns the current progressive enhancement flags
func (k *KeyProtocolState) GetFlags() KeyProtocol {
	return k.Flags
}

// Push pushes the current state onto the stack and sets new flags
func (k *KeyProtocolState) Push(flags KeyProtocol) {
	k.Stack = append(k.Stack, k.Flags)
	k.Flags = flags
}

// Pop restores the previous state from the stack
func (k *KeyProtocolState) Pop() {
	if len(k.Stack) == 0 {
		k.Flags = 0
		return
	}

	k.Flags = k.Stack[len(k.Stack)-1]
	k.Stack = k.Stack[:len(k.Stack)-1]
}

func clampFlags(flags int) int {
	return geom.Clamp(flags, 0, 31)
}

func (t *State) handleKittyProtocol(csi *csiEscape) bool {
	var (
		state         = t.keyState
		args          = csi.args
		intermediates = csi.intermediates
	)

	if len(intermediates) != 1 {
		return false
	}

	switch intermediates[0] {
	case '?': // Query: CSI ? u
		if len(args) > 0 {
			return false
		}

		_, _ = fmt.Fprintf(t.w, "\x1b[?%du", state.Flags)
		return true
	case '=': // Set: CSI = flags; mode u
		if len(args) == 0 || len(args) > 2 {
			return false
		}

		var (
			oldFlags = int(t.keyState.Flags)
			newFlags = clampFlags(args[0])
			mode     = 1
		)

		if len(args) == 2 {
			mode = args[1]
		}

		switch mode {
		case 1:
			// The value 1 means all set bits are set and all unset
			// bits are reset.
			t.keyState.Flags = KeyProtocol(newFlags)
		case 2:
			// The value 2 means all set bits are set, unset bits
			// are left unchanged.
			t.keyState.Flags = KeyProtocol(newFlags | oldFlags)
		case 3:
			// The value 3 means all set bits are reset, unset bits
			// are left unchanged.
			t.keyState.Flags = KeyProtocol((^newFlags) & oldFlags)
		default:
			return false
		}

	case '>': // Push: CSI > flags u
		flags := 0
		if len(args) == 1 {
			flags = clampFlags(args[0])
		}

		// The spec indicates we should cap the size of the stack, but I
		// don't see this as a problem for now. TODO, maybe
		t.keyState.Push(KeyProtocol(flags))
	case '<': // Pop: CSI < count u
		count := 1
		if len(args) == 1 {
			count = geom.Max(args[0], 0)
		}

		for range count {
			t.keyState.Pop()
		}
	default:
		return false
	}

	return true
}

func (t *State) KeyState() KeyProtocol {
	t.RLock()
	defer t.RUnlock()

	if t.keyState == nil {
		return 0
	}

	return t.keyState.GetFlags()
}
