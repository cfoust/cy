//go:build linux
// +build linux

package dir

import (
	"fmt"
	"os"
)

func ForPid(id int) (string, error) {
	cwdPath, err := os.Readlink(fmt.Sprintf("/proc/%d/cwd", id))
	if err != nil {
		return "", err
	}

	return cwdPath, nil
}
