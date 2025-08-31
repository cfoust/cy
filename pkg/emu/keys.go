package emu

import (
	"fmt"
	"strconv"
	"strings"
)

type KeyProtocol int

const (
	KeyLegacy             KeyProtocol = 0
	KeyDisambiguateEscape KeyProtocol = 1 << iota
	KeyReportEventTypes
	KeyReportAlternateKeys
	KeyReportAllKeys
	KeyReportAssociatedText
)

// KeyProtocolState tracks the current state of Kitty protocol support
type KeyProtocolState struct {
	Enabled bool
	Flags   KeyProtocol
	Stack   []KeyProtocol // Stack for push/pop operations
}

// NewKeyProtocolState creates a new Kitty protocol state tracker
func NewKeyProtocolState() *KeyProtocolState {
	return &KeyProtocolState{
		Enabled: false,
		Flags:   0,
		Stack:   make([]KeyProtocol, 0),
	}
}

// IsEnabled returns whether Kitty protocol is currently enabled
func (k *KeyProtocolState) IsEnabled() bool {
	return k.Enabled
}

// GetFlags returns the current progressive enhancement flags
func (k *KeyProtocolState) GetFlags() KeyProtocol {
	return k.Flags
}

// Enable enables Kitty protocol with specified flags
func (k *KeyProtocolState) Enable(flags KeyProtocol) {
	k.Enabled = true
	k.Flags = flags
}

// Disable disables Kitty protocol
func (k *KeyProtocolState) Disable() {
	k.Enabled = false
	k.Flags = 0
}

// Push pushes the current state onto the stack and sets new flags
func (k *KeyProtocolState) Push(flags KeyProtocol) {
	k.Stack = append(k.Stack, k.Flags)
	k.Enabled = true
	k.Flags = flags
}

// Pop restores the previous state from the stack
func (k *KeyProtocolState) Pop() {
	if len(k.Stack) == 0 {
		k.Disable()
		return
	}

	k.Flags = k.Stack[len(k.Stack)-1]
	k.Stack = k.Stack[:len(k.Stack)-1]
	k.Enabled = k.Flags > 0
}

// Query returns the current state as a response string
func (k *KeyProtocolState) Query() string {
	if k.Enabled {
		return fmt.Sprintf("\x1b[?%d;%du", k.Flags, 1)
	}
	return "\x1b[?0u"
}

// HandleKittyProtocolCSI processes Kitty protocol CSI sequences
// Returns true if the sequence was handled, false otherwise
func (t *State) HandleKittyProtocolCSI(csi *csiEscape) bool {
	if t.keyState == nil {
		t.keyState = NewKeyProtocolState()
	}

	// Check for Kitty keyboard protocol sequences
	switch csi.mode {
	case 'u':
		return t.handleKittyKeyboardMode(csi)
	default:
		return false
	}
}

// handleKittyKeyboardMode handles keyboard mode CSI sequences
func (t *State) handleKittyKeyboardMode(csi *csiEscape) bool {
	// For Kitty protocol, we need to reconstruct the sequence content
	// The 'u' mode can have different prefixes: >, <, ?
	var content string
	if csi.priv {
		content = "?"
	}

	// Check if this is a Kitty protocol command (with intermediates)
	if len(csi.intermediates) > 0 {
		content = string(csi.intermediates)
		// Add arguments
		if len(csi.args) > 0 {
			args := make([]string, len(csi.args))
			for i, arg := range csi.args {
				args[i] = fmt.Sprintf("%d", arg)
			}
			content += strings.Join(args, ";")
		}
	}

	// Handle different Kitty keyboard protocol commands
	if strings.HasPrefix(content, ">") {
		// Enable/push protocol: ESC[>{flags};{mode}u
		return t.handleKittyProtocolCommand(content[1:])
	} else if strings.HasPrefix(content, "<") {
		// Disable protocol: ESC[<u
		if t.keyState != nil {
			t.keyState.Disable()
		}
		return true
	} else if strings.HasPrefix(content, "?") {
		// Query protocol: ESC[?u
		if t.keyState != nil {
			response := t.keyState.Query()
			t.w.Write([]byte(response))
		} else {
			t.w.Write([]byte("\x1b[?0u"))
		}
		return true
	}

	return false
}

// handleKittyProtocolCommand handles enable/push commands
func (t *State) handleKittyProtocolCommand(content string) bool {
	parts := strings.Split(content, ";")
	if len(parts) != 2 {
		return false
	}

	flags, err := strconv.Atoi(parts[0])
	if err != nil {
		return false
	}

	mode, err := strconv.Atoi(parts[1])
	if err != nil {
		return false
	}

	if t.keyState == nil {
		t.keyState = NewKeyProtocolState()
	}

	protocol := KeyProtocol(flags)

	switch mode {
	case 1:
		// Enable/set mode
		t.keyState.Enable(protocol)
	case 2:
		// Push current mode and set new
		t.keyState.Push(protocol)
	case 3:
		// Pop previous mode
		t.keyState.Pop()
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
