package main

import (
	"fmt"
	"os"
	"path/filepath"
	"syscall"
)

const (
	CY_SOCKET_ENV      = "CY"
	CY_SOCKET_TEMPLATE = "/tmp/cy-%d"
)

// Much of the socket creation code is ported from tmux. (see tmux.c)
// Part laziness, part I wanted cy to be as familiar as possible.

func getSocketPath() (string, error) {
	uid := os.Getuid()
	directory := fmt.Sprintf(CY_SOCKET_TEMPLATE, uid)

	if err := os.MkdirAll(directory, syscall.S_IRWXU); err != nil {
		return "", err
	}

	info, err := os.Lstat(directory)
	if err != nil {
		return "", err
	}

	if !info.IsDir() {
		return "", fmt.Errorf("%s is not a directory", directory)
	}

	var stat syscall.Stat_t
	err = syscall.Stat(directory, &stat)
	if err != nil {
		return "", err
	}

	if stat.Uid != uint32(uid) || ((stat.Mode & syscall.S_IRWXO) != 0) {
		return "", fmt.Errorf("%s has unsafe permissions", directory)
	}

	label, err := filepath.Abs(filepath.Join(directory, "default"))
	if err != nil {
		return "", err
	}

	return label, nil
}

