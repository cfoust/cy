package params

import (
	"sort"
	"time"

	"github.com/cfoust/cy/pkg/style"
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
	// on startup](/replay-mode.md#recording-to-disk). If
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
	// The number of goroutines to use for searching in .borg files.
	// Defaults to the number of CPUs.
	NumSearchWorkers int
	// Whether to avoid blocking on (input/*) calls. Just for testing.
	skipInput bool

	/////////////////
	// THEME SETTINGS
	/////////////////

	// The format for all timestamps shown in cy. This uses Go's
	// time.Layout format described
	// [here](https://pkg.go.dev/time#Layout).
	TimestampFormat string

	// The [color](/api.md#color) used to represent time mode.
	ReplayTimeFg *style.Color
	// The [color](/api.md#color) used in time mode when the player is playing.
	ReplayPlayFg *style.Color
	// The [color](/api.md#color) used to represent copy mode.
	ReplayCopyFg *style.Color
	// The [color](/api.md#color) used to represent visual mode.
	ReplayVisualFg *style.Color

	// The foreground [color](/api.md#color) of the status bar in replay mode.
	ReplayStatusBarFg *style.Color
	// The background [color](/api.md#color) of the status bar in replay mode.
	ReplayStatusBarBg *style.Color

	// The foreground [color](/api.md#color) of the status bar in search mode.
	SearchStatusBarFg *style.Color
	// The background [color](/api.md#color) of the status bar in search mode.
	SearchStatusBarBg *style.Color
}

var (
	defaults = defaultParams{
		Animate:       true,
		DataDirectory: "",
		DefaultFrame:  "",
		DefaultShell:  "/bin/bash",
		skipInput:     false,

		TimestampFormat:   time.DateTime,
		ReplayTimeFg:      style.NewColor("4"),
		ReplayPlayFg:      style.NewColor("12"),
		ReplayCopyFg:      style.NewColor("3"),
		ReplayVisualFg:    style.NewColor("10"),
		ReplayStatusBarFg: style.NewColor("15"),
		ReplayStatusBarBg: style.NewColor("8"),

		SearchStatusBarFg: style.NewColor("15"),
		SearchStatusBarBg: style.NewColor("4"),
	}
)
