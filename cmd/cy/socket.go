package main

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/cfoust/cy/pkg/sessions"
)

const (
	CY_SOCKET_ENV      = "CY"
	CY_SOCKET_TEMPLATE = "/tmp/cy-%d"
)

// Much of the socket creation code is ported from tmux. (see tmux.c)
// Part laziness, part I wanted cy to be as familiar as possible.

func getSocketPath(name string) (string, error) {
	uid := os.Getuid()
	directory := fmt.Sprintf(CY_SOCKET_TEMPLATE, uid)

	if err := sessions.EnsureDirectory(directory); err != nil {
		return "", err
	}

	label, err := filepath.Abs(filepath.Join(directory, name))
	if err != nil {
		return "", err
	}

	return label, nil
}
