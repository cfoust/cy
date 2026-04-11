package api

import (
	"os"
	"path/filepath"
	"strings"
)

type PathModule struct{}

func (p *PathModule) Abs(path string) (string, error) {
	return filepath.Abs(path)
}

func (p *PathModule) Base(path string) string {
	return filepath.Base(path)
}

func (p *PathModule) Join(elem []string) string {
	return filepath.Join(elem...)
}

func (p *PathModule) Glob(pattern string) ([]string, error) {
	return filepath.Glob(pattern)
}

// Expand replaces a leading `~` or `~/` in path with the current user's
// home directory. Paths that do not start with `~` are returned
// unchanged.
func (p *PathModule) Expand(path string) (string, error) {
	if path != "~" && !strings.HasPrefix(path, "~/") {
		return path, nil
	}

	home, err := os.UserHomeDir()
	if err != nil {
		return "", err
	}

	if path == "~" {
		return home, nil
	}

	return filepath.Join(home, path[2:]), nil
}
