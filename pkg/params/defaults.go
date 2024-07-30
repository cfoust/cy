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
	// A list of all of the enabled animations that will be used by
	// (input/find). If this is an empty array, all built-in animations
	// will be enabled.
	Animations []string
	// The directory in which .borg files will be saved. This is [inferred
	// on startup](/replay-mode.md#recording-terminal-sessions-to-disk). If
	// set to an empty string, recording to disk is disabled.
	DataDirectory string
	// The default shell with which to start panes. Defaults to the value
	// of `$SHELL` on startup.
	DefaultShell string
	// The frame used for all new clients. A blank string means a random
	// frame will be chosen from all frames.
	DefaultFrame string
	// If this is `true`, when a pane's process exits or its node is killed
	// (such as with {{api tree/kill}}), the portion of the layout related
	// to that node will be removed. This makes cy's layout functionality
	// work a bit more like tmux.
	RemovePaneOnExit bool
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
