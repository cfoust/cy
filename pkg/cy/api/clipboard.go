package api

import (
	"github.com/cfoust/cy/pkg/clipboard"
)

type ClipboardModule struct {
	Clipboard clipboard.Clipboard
}

func (c *ClipboardModule) Set(context interface{}, text string) error {
	client, err := getClient(context)
	if err != nil {
		return err
	}

	if client.Params().UseSystemClipboard() {
		return c.Clipboard.Write(text)
	}

	return client.Clipboard().Write(text)
}

func (c *ClipboardModule) Get(context interface{}) (string, error) {
	client, err := getClient(context)
	if err != nil {
		return "", err
	}

	if client.Params().UseSystemClipboard() {
		return c.Clipboard.Read()
	}

	return client.Clipboard().Read()
}
