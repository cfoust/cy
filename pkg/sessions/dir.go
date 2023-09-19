package sessions

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync/atomic"
	"syscall"
	"time"
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
		// TODO(cfoust): 09/19/23 this should just be a warning for
		// recording sessions
		return fmt.Errorf("%s has unsafe permissions", path)
	}

	return nil
}

var (
	sessionId atomic.Int32
)

func GetFilename(dataDir string, path string) (string, error) {
	if err := EnsureDirectory(dataDir); err != nil {
		return "", err
	}

	cleanPath, err := filepath.Abs(filepath.Clean(path))
	if err != nil {
		return "", err
	}

	return filepath.Join(
		dataDir,
		fmt.Sprintf(
			"%s-%d-%s.borg",
			time.Now().Format("2006.01.02.03.04.05"),
			sessionId.Add(1),
			strings.ReplaceAll(
				cleanPath,
				string(filepath.Separator),
				"%",
			),
		),
	), nil
}
