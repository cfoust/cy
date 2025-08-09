package api

import (
	"fmt"

	"github.com/cfoust/cy/pkg/clipboard"
)

type RegisterModule struct {
	Clipboard clipboard.Clipboard
}

func (c *RegisterModule) Set(
	register string,
	text string,
) error {
	if register == "+" {
		return c.Clipboard.Write(text)
	}
	return fmt.Errorf(
		"register '%s' is not supported in Go API, use Janet (register/*) functions",
		register,
	)
}

func (c *RegisterModule) Get(
	register string,
) (string, error) {
	if register == "+" {
		return c.Clipboard.Read()
	}
	return "", fmt.Errorf(
		"register '%s' is not supported in Go API, use Janet (register/*) functions",
		register,
	)
}
