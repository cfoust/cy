package main

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

func findConfig() string {
	roots := make([]string, 0)

	// $XDG_CONFIG_HOME/cy/cyrc.janet
	// $XDG_CONFIG_HOME/cy.janet
	// $HOME/.config/cy/cy.janet
	// $HOME/cy/cy.janet
	// $HOME/.cy.janet

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
	}

	return ""
}

func findDataDir() string {
	if xdgData, ok := os.LookupEnv("XDG_DATA_HOME"); ok {
		return filepath.Join(xdgData, "cy")
	}
	home, ok := os.LookupEnv("HOME")
	if !ok {
		home = "~"
	}

	return filepath.Join(home, ".local", "share", "cy")
}
