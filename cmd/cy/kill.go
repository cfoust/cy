package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"syscall"
)

func killServerCommand() error {
	socketPath, err := getSocketPath(CLI.Socket)
	if err != nil {
		return fmt.Errorf(
			"failed to detect socket path: %s",
			err,
		)
	}

	pidPath := getPidPath(socketPath)
	data, err := os.ReadFile(pidPath)
	if err != nil {
		return fmt.Errorf(
			"no server running (could not read %s)",
			pidPath,
		)
	}

	pid, err := strconv.Atoi(strings.TrimSpace(string(data)))
	if err != nil {
		return fmt.Errorf(
			"invalid PID in %s: %s",
			pidPath,
			err,
		)
	}

	process, err := os.FindProcess(pid)
	if err != nil {
		return fmt.Errorf(
			"could not find process %d: %s",
			pid,
			err,
		)
	}

	if err := process.Signal(syscall.SIGTERM); err != nil {
		return fmt.Errorf(
			"failed to kill server (pid %d): %s",
			pid,
			err,
		)
	}

	_ = os.Remove(pidPath)
	_ = os.Remove(socketPath)

	return nil
}
