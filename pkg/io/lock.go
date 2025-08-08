package io

import (
	"fmt"
	"os"
	"syscall"
)

var ErrorLockFailed = fmt.Errorf("failed to lock file")

func Lock(lockPath string) (*os.File, error) {
	fd, err := os.OpenFile(lockPath, os.O_WRONLY|os.O_CREATE, 0600)
	if err != nil {
		return nil, err
	}

	if err := syscall.Flock(int(fd.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		_ = fd.Close()
		return nil, ErrorLockFailed
	}

	return fd, nil
}
