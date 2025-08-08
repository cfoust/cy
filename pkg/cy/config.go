package cy

import (
	"os"
	"path/filepath"
)

func fileExists(path string) bool {
	if _, err := os.Stat(path); !os.IsNotExist(err) {
		return true
	}
	return false
}

// TODO(cfoust): 09/17/23 support XDG_CONFIG_DIRS and XDG_DATA_DIRS

func FindConfig() string {
	roots := make([]string, 0)

	if xdgConfig, ok := os.LookupEnv("XDG_CONFIG_HOME"); ok {
		roots = append(roots, xdgConfig)
	}

	if home, ok := os.LookupEnv("HOME"); ok {
		roots = append(
			roots,
			home,
			filepath.Join(home, ".config"),
		)
	}

	for _, root := range roots {
		if path := filepath.Join(root, "cy", "cyrc.janet"); fileExists(path) {
			return path
		}

		if path := filepath.Join(root, "cyrc.janet"); fileExists(path) {
			return path
		}

		if path := filepath.Join(root, ".cy.janet"); fileExists(path) {
			return path
		}
	}

	return ""
}

func FindDataDir() string {
	if xdgData, ok := os.LookupEnv("XDG_DATA_HOME"); ok {
		return filepath.Join(xdgData, "cy")
	}
	home, ok := os.LookupEnv("HOME")
	if !ok {
		home = "~"
	}

	return filepath.Join(home, ".local", "share", "cy")
}

func FindStateDir() string {
	if xdgState, ok := os.LookupEnv("XDG_STATE_HOME"); ok {
		return filepath.Join(xdgState, "cy")
	}
	home, ok := os.LookupEnv("HOME")
	if !ok {
		home = "~"
	}

	return filepath.Join(home, ".local", "state", "cy")
}
