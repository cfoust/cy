package params

import (
	"sort"
)

type DefaultParam struct {
	Name      string
	Docstring string
	Default   interface{}
}

var _defaultParams []DefaultParam

func DefaultParams() []DefaultParam {
	params := make([]DefaultParam, len(_defaultParams))
	copy(params, _defaultParams)
	sort.SliceStable(params, func(i, j int) bool {
		return params[i].Name < params[j].Name
	})
	return params
}

//go:generate go run gen.go
type defaultParams struct {
	// Whether to enable animation.
	Animate bool
	// The directory in which .borg files will be saved. This is [inferred
	// on startup](replay-mode.md#recording-terminal-sessions-to-disk). If
	// set to an empty string, recording is disabled.
	DataDirectory string
	// The default shell with which to start panes. Defaults to the value
	// of `$SHELL` on startup.
	DefaultShell string
	// The frame used for all new clients. A blank string means a random
	// frame will be chosen from all frames.
	DefaultFrame string
	// Whether to avoid blocking on (input/*) calls. Just for testing.
	skipInput bool
}

var (
	defaults = defaultParams{
		Animate:       true,
		DataDirectory: "",
		DefaultFrame:  "",
		DefaultShell:  "/bin/bash",
		skipInput:     false,
	}
)
