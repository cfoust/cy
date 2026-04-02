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

// xdgDirs returns the directories from the given XDG environment
// variable, falling back to defaultDirs if the variable is unset or
// empty.
func xdgDirs(envVar string, defaultDirs string) []string {
	var raw []string
	if dirs, ok := os.LookupEnv(envVar); ok && dirs != "" {
		raw = filepath.SplitList(dirs)
	} else if defaultDirs != "" {
		raw = filepath.SplitList(defaultDirs)
	}

	// Per the XDG spec, relative paths must be ignored.
	result := make([]string, 0, len(raw))
	for _, dir := range raw {
		if filepath.IsAbs(dir) {
			result = append(result, dir)
		}
	}
	return result
}

func FindConfig() (string, bool) {
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

	roots = append(roots, xdgDirs("XDG_CONFIG_DIRS", "/etc/xdg")...)

	for _, root := range roots {
		if path := filepath.Join(root, "cy", "cyrc.janet"); fileExists(path) {
			return path, true
		}

		if path := filepath.Join(root, "cyrc.janet"); fileExists(path) {
			return path, true
		}

		if path := filepath.Join(root, ".cy.janet"); fileExists(path) {
			return path, true
		}
	}

	return "", false
}

func FindDataDir() string {
	if xdgData, ok := os.LookupEnv("XDG_DATA_HOME"); ok {
		return filepath.Join(xdgData, "cy")
	}

	for _, dir := range xdgDirs(
		"XDG_DATA_DIRS",
		"/usr/local/share:/usr/share",
	) {
		path := filepath.Join(dir, "cy")
		if info, err := os.Stat(path); err == nil && info.IsDir() {
			return path
		}
	}

	home, ok := os.LookupEnv("HOME")
	if !ok {
		home = "~"
	}

	return filepath.Join(home, ".local", "share", "cy")
}

func FindPluginDir() string {
	candidates := make([]string, 0)

	if xdgConfig, ok := os.LookupEnv("XDG_CONFIG_HOME"); ok {
		candidates = append(
			candidates,
			filepath.Join(xdgConfig, "cy", "plugins"),
		)
	}

	if home, ok := os.LookupEnv("HOME"); ok {
		candidates = append(
			candidates,
			filepath.Join(home, ".cy", "plugins"),
			filepath.Join(home, ".config", "cy", "plugins"),
		)
	}

	for _, dir := range xdgDirs("XDG_CONFIG_DIRS", "/etc/xdg") {
		candidates = append(
			candidates,
			filepath.Join(dir, "cy", "plugins"),
		)
	}

	for _, dir := range candidates {
		if info, err := os.Stat(dir); err == nil && info.IsDir() {
			return dir
		}
	}

	return ""
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
