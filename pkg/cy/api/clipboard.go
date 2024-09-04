package api

import (
	"fmt"
	"golang.design/x/clipboard"
)

type ClipboardModule struct {
}

func (c *ClipboardModule) Init() error {
	// Init returns an error if the package is not ready for use.
	err := clipboard.Init()
	if err != nil {
		return fmt.Errorf("clipboard can not init")
	}
}

func (c *ClipboardModule) Set(text []byte) error {
	clipboard.Write(clipboard.FmtText, []byte(text))
}

func (c *ClipboardModule) Get() []byte {
	return clipboard.Read(clipboard.FmtText)
}
