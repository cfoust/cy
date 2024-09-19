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

	// The [color](/api.md#color) used for info messages.
	ColorInfo *style.Color
	// The [color](/api.md#color) used for warning messages.
	ColorWarn *style.Color
	// The [color](/api.md#color) used for error messages.
	ColorError *style.Color

	// The format for all timestamps shown in cy. This uses Go's
	// time.Layout format described
	// [here](https://pkg.go.dev/time#Layout).
	TimestampFormat string

	// The [color](/api.md#color) of the preview border in (input/find).
	InputPreviewBorderFg *style.Color
	// The border style of the preview border in (input/find).
	InputPreviewBorder *style.Border
	// The foreground [color](/api.md#color) of the input prompt in
	// (input/*) functions.
	InputPromptFg *style.Color
	// The background [color](/api.md#color) of the input prompt in
	// (input/*) functions.
	InputPromptBg *style.Color
	// The foreground [color](/api.md#color) of the active row in (input/find).
	InputFindActiveFg *style.Color
	// The background [color](/api.md#color) of the active row in (input/find).
	InputFindActiveBg *style.Color
	// The foreground [color](/api.md#color) of the inactive row in (input/find).
	InputFindInactiveFg *style.Color
	// The background [color](/api.md#color) of the inactive row in (input/find).
	InputFindInactiveBg *style.Color

	// The foreground [color](/api.md#color) used to represent time mode.
	ReplayTimeFg *style.Color
	// The [color](/api.md#color) used to represent time mode.
	ReplayTimeBg *style.Color
	// The foreground [color](/api.md#color) used in time mode when the player is playing.
	ReplayPlayFg *style.Color
	// The [color](/api.md#color) used in time mode when the player is playing.
	ReplayPlayBg *style.Color
	// The foreground [color](/api.md#color) used in time mode when the player is playing.
	ReplayCopyFg *style.Color
	// The [color](/api.md#color) used to represent copy mode.
	ReplayCopyBg *style.Color
	// The foreground [color](/api.md#color) used in time mode when the player is playing.
	ReplayVisualFg *style.Color
	// The [color](/api.md#color) used to represent visual mode.
	ReplayVisualBg *style.Color

	// The foreground [color](/api.md#color) for selections in replay mode.
	ReplaySelectionFg *style.Color
	// The background [color](/api.md#color) for selections in replay mode.
	ReplaySelectionBg *style.Color
	// The foreground [color](/api.md#color) for incremental search in replay mode.
	ReplayIncrementalFg *style.Color
	// The background [color](/api.md#color) for incremental search in replay mode.
	ReplayIncrementalBg *style.Color

	// The text shown in the status bar when in time mode.
	ReplayTextTimeMode string
	// The text shown in the status bar when playing.
	ReplayTextPlayMode string
	// The text shown in the status bar when in copy mode.
	ReplayTextCopyMode string
	// The text shown in the status bar when in visual mode.
	ReplayTextVisualMode string

	// The foreground [color](/api.md#color) of the status bar in replay mode.
	ReplayStatusBarFg *style.Color
	// The background [color](/api.md#color) of the status bar in replay mode.
	ReplayStatusBarBg *style.Color

	// The foreground [color](/api.md#color) of the status bar in search mode.
	SearchStatusBarFg *style.Color
	// The background [color](/api.md#color) of the status bar in search mode.
	SearchStatusBarBg *style.Color
	// The text shown in the status bar when searching.
	SearchTextSearching string
	// The text shown in the status bar when no matches are found.
	SearchTextNoMatchesFound string
}

var (
	defaults = defaultParams{
		Animate:       true,
		DataDirectory: "",
		DefaultFrame:  "",
		DefaultShell:  "/bin/bash",
		skipInput:     false,

		ColorInfo:  style.NewColor("6"),
		ColorWarn:  style.NewColor("3"),
		ColorError: style.NewColor("1"),

		TimestampFormat:          time.DateTime,
		InputPreviewBorderFg:     style.NewColor("5"),
		InputPreviewBorder:       &style.Borders[0],
		InputPromptFg:            style.NewColor("0"),
		InputPromptBg:            style.NewColor("3"),
		InputFindActiveFg:        style.NewColor("0"),
		InputFindActiveBg:        style.NewColor("15"),
		InputFindInactiveFg:      style.NewColor("0"),
		InputFindInactiveBg:      style.NewColor("7"),
		ReplayTimeFg:             style.NewColor("15"),
		ReplayTimeBg:             style.NewColor("4"),
		ReplayPlayFg:             style.NewColor("15"),
		ReplayPlayBg:             style.NewColor("12"),
		ReplayCopyFg:             style.NewColor("0"),
		ReplayCopyBg:             style.NewColor("3"),
		ReplayVisualFg:           style.NewColor("0"),
		ReplayVisualBg:           style.NewColor("10"),
		ReplayStatusBarFg:        style.NewColor("15"),
		ReplayStatusBarBg:        style.NewColor("8"),
		ReplaySelectionFg:        style.NewColor("9"),
		ReplaySelectionBg:        style.NewColor("8"),
		ReplayIncrementalFg:      style.NewColor("0"),
		ReplayIncrementalBg:      style.NewColor("3"),
		ReplayTextTimeMode:       "⏵",
		ReplayTextPlayMode:       "⏸",
		ReplayTextCopyMode:       "COPY",
		ReplayTextVisualMode:     "VISUAL",
		SearchStatusBarFg:        style.NewColor("15"),
		SearchStatusBarBg:        style.NewColor("4"),
		SearchTextSearching:      "searching",
		SearchTextNoMatchesFound: "no matches found for",
	}
)
