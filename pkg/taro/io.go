package taro

import (
	"io"

	"github.com/cfoust/cy/pkg/keys"

	tea "github.com/charmbracelet/bubbletea"
)

type KeyMsg = tea.KeyMsg
type KittyKeyMsg keys.Key

func (k KittyKeyMsg) Tea() (tea.KeyMsg, bool) {
	return keys.Key(k).Tea()
}

// MouseMsg contains information about a mouse event and are sent to a programs
// update function when mouse activity occurs. Note that the mouse must first
// be enabled in order for the mouse events to be received.
type MouseMsg keys.Mouse

// String returns a string representation of a mouse event.
func (m MouseMsg) String() string {
	return keys.Mouse(m).String()
}

func (m MouseMsg) Bytes() []byte {
	return keys.Mouse(m).Bytes()
}

func DetectOneMsg(b []byte) (msg Msg, w int) {
	var event any
	event, w = keys.Read(b)
	switch event := event.(type) {
	case keys.Mouse:
		msg = MouseMsg(event)
	case keys.Key:
		msg = KittyKeyMsg(event)
	default:
		msg = event
	}

	return msg, w
}

// readInputs reads keypress and mouse inputs from a TTY and produces messages
// containing information about the key or mouse events accordingly.
func readInputs(input io.Reader) (msgs []Msg, err error) {
	var buf [256]byte

	// Read and block.
	numBytes, err := input.Read(buf[:])
	if err != nil {
		return nil, err
	}
	b := buf[:numBytes]

	var i, w int
	for i = 0; i < len(b); i += w {
		var msg Msg
		msg, w = DetectOneMsg(b[i:])
		msgs = append(msgs, msg)
	}

	return
}

func TranslateMouseMessage(msg Msg, dx, dy int) Msg {
	switch msg := msg.(type) {
	case MouseMsg:
		cloned := msg
		cloned.C += dx
		cloned.R += dy
		return cloned
	}
	return msg
}

func KeysToMsg(strs ...string) (msgs []KittyKeyMsg) {
	for _, s := range strs {
		key, ok := keys.FromHuman(s)
		if !ok {
			continue
		}
		msgs = append(msgs, KittyKeyMsg(key))
	}

	return
}
