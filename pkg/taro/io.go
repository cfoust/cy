package taro

import (
	"io"

	"github.com/cfoust/cy/pkg/keys"

	tea "github.com/charmbracelet/bubbletea"
)

type KeyMsg keys.Key

func (k KeyMsg) ToTea() tea.KeyMsg {
	key := keys.Key(k)
	legacyType := key.toLegacyKeyType()

	var runes []rune
	if legacyType == 0 && len(key.Runes) > 0 && key.Runes[0] <= 0x10FFFF &&
		key.Runes[0] > 31 &&
		key.Runes[0] != 127 {
		runes = []rune{rune(key.Runes[0])}
		legacyType = tea.KeyRunes
	}

	return tea.KeyMsg{
		Type:  tea.KeyType(legacyType),
		Runes: runes,
		Alt:   key.Mod&keys.KeyModAlt != 0,
	}
}

// String returns a string representation for a key message. It's safe (and
// encouraged) for use in key comparison.
func (k KeyMsg) String() (str string) {
	return keys.Key(k).String()
}

// MouseMsg contains information about a mouse event and are sent to a programs
// update function when mouse activity occurs. Note that the mouse must first
// be enabled in order for the mouse events to be received.
type MouseMsg keys.MouseEvent

// String returns a string representation of a mouse event.
func (m MouseMsg) String() string {
	return keys.MouseEvent(m).String()
}

func (m MouseMsg) Bytes() []byte {
	return keys.MouseEvent(m).Bytes()
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
		w, msg = keys.DetectOneMsg(b[i:])
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
