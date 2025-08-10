package clipboard

import (
	"encoding/base64"
	"fmt"
	"io"

	"github.com/sasha-s/go-deadlock"
	"golang.design/x/clipboard"
)

type Clipboard interface {
	Write(string) error
	Read() (string, error)
}

type MemoryClipboard struct {
	deadlock.RWMutex
	clipboard string
}

var _ Clipboard = (*MemoryClipboard)(nil)

func (mc *MemoryClipboard) Write(text string) error {
	mc.Lock()
	mc.clipboard = text
	mc.Unlock()
	return nil
}

func (mc *MemoryClipboard) Read() (string, error) {
	mc.RLock()
	defer mc.RUnlock()
	return mc.clipboard, nil
}

type SystemClipboard struct {
}

func (c *SystemClipboard) Write(text string) error {
	return clipboard.Write(clipboard.FmtText, []byte(text))
}

func (c *SystemClipboard) Read() (string, error) {
	buf, err := clipboard.ReadErr(clipboard.FmtText)
	if err != nil {
		return "", err
	}

	return string(buf), nil
}

// Init returns an error if the package is not ready for use.
func (c *SystemClipboard) init() error {
	err := clipboard.Init()
	if err == nil {
		return nil
	}

	return fmt.Errorf("clipboard failed to initialize: %w", err)
}

func NewSystemClipboard() (*SystemClipboard, error) {
	s := &SystemClipboard{}
	err := s.init()
	if err != nil {
		return nil, err
	}

	return s, nil
}

type OSC52Clipboard struct {
	w io.Writer
}

var _ Clipboard = (*OSC52Clipboard)(nil)

func NewOSC52Clipboard(writer io.Writer) *OSC52Clipboard {
	return &OSC52Clipboard{
		w: writer,
	}
}

func (c *OSC52Clipboard) Write(text string) error {
	// OSC-52 format: \033]52;c;<base64-encoded-text>\007
	// where 'c' means clipboard
	encoded := base64.StdEncoding.EncodeToString([]byte(text))
	sequence := fmt.Sprintf("\033]52;c;%s\007", encoded)

	_, err := c.w.Write([]byte(sequence))
	if err != nil {
		return fmt.Errorf("failed to write OSC-52 sequence: %w", err)
	}

	return nil
}

func (c *OSC52Clipboard) Read() (string, error) {
	return "", nil
}
