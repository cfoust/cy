package main

import (
	"os"
	"path/filepath"
)

/*$XDG_CONFIG_HOME/cy/cyrc.janet*/
/*$XDG_CONFIG_HOME/cy.janet*/
/*$HOME/.config/cy/cy.janet*/
/*$HOME/cy/cy.janet*/
/*$HOME/.cy.janet*/

func fileExists(path string) bool {
	if _, err := os.Stat(path); !os.IsNotExist(err) {
		return true
	}
	return false
}

func findConfig() (result string, found bool) {
	roots := make([]string, 0)

	if xdgConfig, ok := os.LookupEnv("XDG_CONFIG_HOME"); ok {
		roots = append(roots, xdgConfig)
	}

	if home, ok := os.LookupEnv("HOME"); ok {
		roots = append(roots, home)
	}

	for _, root := range roots {
		if path := filepath.Join(root, "cy", "cyrc.janet"); fileExists(path) {
			return path, true
		}

		if path := filepath.Join(root, "cyrc.janet"); fileExists(path) {
			return path, true
		}
	}

	return
}
