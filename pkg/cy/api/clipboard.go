package api

import (
	"github.com/cfoust/cy/pkg/clipboard"
)

type ClipboardModule struct {
	Clipboard clipboard.Clipboard
}

func (c *ClipboardModule) Set(text string) error {
	return c.Clipboard.Write(text)
}

func (c *ClipboardModule) Get() (string, error) {
	return c.Clipboard.Read()
}
