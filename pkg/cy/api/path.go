package api

import (
	"path/filepath"
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
