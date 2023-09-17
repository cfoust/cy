package sessions

import (
	"fmt"
	"os"
	"syscall"
)

// EnsureDirectory creates a directory if it does not exist and checks whether
// other users can read and write files in it.
func EnsureDirectory(path string) error {
	uid := os.Getuid()

	if err := os.MkdirAll(path, syscall.S_IRWXU); err != nil {
		return err
	}

	info, err := os.Lstat(path)
	if err != nil {
		return err
	}

	if !info.IsDir() {
		return fmt.Errorf("%s is not a directory", path)
	}

	var stat syscall.Stat_t
	err = syscall.Stat(path, &stat)
	if err != nil {
		return err
	}

	if stat.Uid != uint32(uid) || ((stat.Mode & syscall.S_IRWXO) != 0) {
		return fmt.Errorf("%s has unsafe permissions", path)
	}

	return nil
}
