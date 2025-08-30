package emu

import (
	"fmt"
	"strconv"
	"strings"
)

// KittyProtocolState tracks the current state of Kitty protocol support
type KittyProtocolState struct {
	Enabled bool
	Flags   int
	Stack   []int // Stack for push/pop operations
}

// NewKittyProtocolState creates a new Kitty protocol state tracker
func NewKittyProtocolState() *KittyProtocolState {
	return &KittyProtocolState{
		Enabled: false,
		Flags:   0,
		Stack:   make([]int, 0),
	}
}

// IsEnabled returns whether Kitty protocol is currently enabled
func (k *KittyProtocolState) IsEnabled() bool {
	return k.Enabled
}

// GetFlags returns the current progressive enhancement flags
func (k *KittyProtocolState) GetFlags() int {
	return k.Flags
}

// Enable enables Kitty protocol with specified flags
func (k *KittyProtocolState) Enable(flags int) {
	k.Enabled = true
	k.Flags = flags
}

// Disable disables Kitty protocol
func (k *KittyProtocolState) Disable() {
	k.Enabled = false
	k.Flags = 0
}

// Push pushes the current state onto the stack and sets new flags
func (k *KittyProtocolState) Push(flags int) {
	k.Stack = append(k.Stack, k.Flags)
	k.Enabled = true
	k.Flags = flags
}

// Pop restores the previous state from the stack
func (k *KittyProtocolState) Pop() {
	if len(k.Stack) == 0 {
		k.Disable()
		return
	}

	k.Flags = k.Stack[len(k.Stack)-1]
	k.Stack = k.Stack[:len(k.Stack)-1]
	k.Enabled = k.Flags > 0
}

// Query returns the current state as a response string
func (k *KittyProtocolState) Query() string {
	if k.Enabled {
		return fmt.Sprintf("\x1b[?%d;%du", k.Flags, 1)
	}
	return "\x1b[?0u"
}

// HandleKittyProtocolCSI processes Kitty protocol CSI sequences
// Returns true if the sequence was handled, false otherwise
func (t *State) HandleKittyProtocolCSI(csi *csiEscape) bool {
	if t.kittyState == nil {
		t.kittyState = NewKittyProtocolState()
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
		if t.kittyState != nil {
			t.kittyState.Disable()
		}
		return true
	} else if strings.HasPrefix(content, "?") {
		// Query protocol: ESC[?u
		if t.kittyState != nil {
			response := t.kittyState.Query()
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

	if t.kittyState == nil {
		t.kittyState = NewKittyProtocolState()
	}

	switch mode {
	case 1:
		// Enable/set mode
		t.kittyState.Enable(flags)
	case 2:
		// Push current mode and set new
		t.kittyState.Push(flags)
	case 3:
		// Pop previous mode
		t.kittyState.Pop()
	default:
		return false
	}

	return true
}

// ShouldUseKittyProtocol returns whether to use Kitty protocol for key encoding
func (t *State) ShouldUseKittyProtocol() bool {
	return t.kittyState != nil && t.kittyState.IsEnabled()
}

// GetKittyFlags returns the current Kitty protocol flags
func (t *State) GetKittyFlags() int {
	if t.kittyState == nil {
		return 0
	}
	return t.kittyState.GetFlags()
}
