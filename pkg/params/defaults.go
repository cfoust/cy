package params

import (
	"sort"
	"time"

	"github.com/cfoust/cy/pkg/style"
)

type DefaultParam struct {
	Name      string
	Docstring string
	Default   any
}

var _defaultParams []DefaultParam
var _params = New()

func DefaultParams() []DefaultParam {
	params := make([]DefaultParam, len(_defaultParams))
	copy(params, _defaultParams)
	sort.SliceStable(params, func(i, j int) bool {
		return params[i].Name < params[j].Name
	})
	return params
}

// IsDefaultParam reports whether the given key is a default parameter.
func IsDefaultParam(key string) bool {
	return _params.isDefault(key)
}

//go:generate go run gen.go
type defaultParams struct {
	// Whether to enable animation.
	Animate bool
	// The delay (in seconds) before animations should begin. This is useful
	// if you like cy's animations but find them distracting when working
	// quickly. This only applies to the animations that play when using the
	// (input/*) family of functions.
	AnimationDelay int
	// A list of all of the enabled animations that will be used by
	// (input/find). If this is an empty array, all built-in animations
	// will be enabled.
	Animations []string
	// The target frames per second (FPS) for animations.
	AnimationFps int
	// The directory in which .borg files will be saved. This is [inferred
	// on startup](/replay-mode.md#recording-to-disk). If
	// set to an empty string, recording to disk is disabled.
	DataDirectory string
	// The maximum number of physical lines kept in memory for a pane's
	// scrollback buffer. When recording to disk, this is the amount of
	// scrollback available in copy mode before cy loads the full history from
	// the .borg file. A non-positive value disables pruning.
	HistoryLimit int
	// The default shell with which to start panes. Defaults to the value
	// of `$SHELL` on startup.
	DefaultShell string
	// The frame used for all new clients. A blank string means a random
	// frame will be chosen from all frames.
	DefaultFrame string
	// If this is `true`, when a pane's process exits or its node is killed
	// (such as with {{api tree/rm}}), the portion of the layout related
	// to that node will be removed. This makes cy's layout functionality
	// work a bit more like tmux.
	RemovePaneOnExit bool
	// The number of goroutines to use for searching in .borg files.
	// Defaults to the number of CPUs.
	NumSearchWorkers int
	// Whether to use the system clipboard instead of outputting OSC-52
	// codes. This should generally be "true" unless you use an old terminal
	// emulator.
	UseSystemClipboard bool
	// Whether to avoid blocking on (input/*) calls. Just for testing.
	skipInput bool

	/////////////////
	// THEME SETTINGS
	/////////////////

	// The [color map](/api.md#color-map) used to translate the colors
	// used for rendering a pane.
	ColorMap *style.ColorMap

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
	// The [style](/api.md#style) of the input prompt in
	// (input/*) functions.
	InputPromptStyle *style.Style
	// The [style](/api.md#style) of the active row in (input/find).
	InputFindActiveStyle *style.Style
	// The [style](/api.md#style) of the inactive row in (input/find).
	InputFindInactiveStyle *style.Style
	// The [style](/api.md#style) of highlighted characters in (input/find).
	InputFindHighlightStyle *style.Style
	// The [style](/api.md#style) of match text in (input/thumbs).
	InputThumbsMatchStyle *style.Style
	// The [style](/api.md#style) of hint text in (input/thumbs).
	InputThumbsHintStyle *style.Style
	// The [style](/api.md#style) of partially selected hint text in (input/thumbs).
	InputThumbsPartialStyle *style.Style

	// The [style](/api.md#style) used to represent time mode.
	ReplayTimeStyle *style.Style
	// The [style](/api.md#style) used in time mode when the player is playing.
	ReplayPlayStyle *style.Style
	// The [style](/api.md#style) used to represent copy mode.
	ReplayCopyStyle *style.Style
	// The [style](/api.md#style) used to represent visual mode.
	ReplayVisualStyle *style.Style

	// The [style](/api.md#style) for selections in replay mode.
	ReplaySelectionStyle *style.Style
	// The [style](/api.md#style) for incremental search in replay mode.
	ReplayIncrementalStyle *style.Style
	// The [style](/api.md#style) for the current search match in replay mode.
	ReplayMatchActiveStyle *style.Style
	// The [style](/api.md#style) for search matches in replay mode.
	ReplayMatchInactiveStyle *style.Style

	// The text shown in the status bar when in time mode.
	ReplayTextTimeMode string
	// The text shown in the status bar when playing.
	ReplayTextPlayMode string
	// The text shown in the status bar when in copy mode.
	ReplayTextCopyMode string
	// The text shown in the status bar when in visual mode.
	ReplayTextVisualMode string
	// The text shown in the status bar when in visual line mode.
	ReplayTextVisualLineMode string

	// The [style](/api.md#style) of the status bar in replay mode.
	ReplayStatusBarStyle *style.Style

	// The [style](/api.md#style) of the status bar in search mode.
	SearchStatusBarStyle *style.Style
	// The text shown in the status bar when searching.
	SearchTextSearching string
	// The text shown in the status bar when no matches are found.
	SearchTextNoMatchesFound string
	// The text shown when a terminal session exits.
	TerminalTextExited string
}

var (
	defaults = defaultParams{
		Animate:            true,
		AnimationDelay:     0,
		AnimationFps:       23,
		DataDirectory:      "",
		HistoryLimit:       5000,
		DefaultFrame:       "",
		DefaultShell:       "/bin/bash",
		skipInput:          false,
		UseSystemClipboard: false,

		ColorMap: &style.ColorMap{},

		ColorInfo:  style.Cyan,
		ColorWarn:  style.Yellow,
		ColorError: style.Red,

		TimestampFormat:         time.DateTime,
		InputPreviewBorderFg:    style.Magenta,
		InputPreviewBorder:      &style.Borders[0],
		InputPromptStyle:        style.NewStyle(style.Black, style.Yellow),
		InputFindActiveStyle:    style.NewStyle(style.Black, style.White),
		InputFindInactiveStyle:  style.NewStyle(style.Black, style.LightGrey),
		InputFindHighlightStyle: style.NewStyle(style.White, style.Red),
		InputThumbsMatchStyle:   style.NewStyle(style.Red, style.Black),
		InputThumbsHintStyle:    style.NewStyle(style.Yellow, style.Black),
		InputThumbsPartialStyle: style.NewStyle(style.White, style.Blue),
		ReplayTimeStyle:         style.NewStyle(style.White, style.Blue),
		ReplayPlayStyle:         style.NewStyle(style.White, style.LightBlue),
		ReplayCopyStyle:         style.NewStyle(style.Black, style.Yellow),
		ReplayVisualStyle:       style.NewStyle(style.Black, style.LightGreen),
		ReplayStatusBarStyle:    style.NewStyle(style.White, style.DarkGrey),
		ReplaySelectionStyle: style.NewStyle(
			style.LightRed,
			style.DarkGrey,
		),
		ReplayIncrementalStyle:   style.NewStyle(style.Black, style.Yellow),
		ReplayMatchActiveStyle:   style.NewStyle(style.Red, style.LightMagenta),
		ReplayMatchInactiveStyle: style.NewStyle(style.Red, style.LightCyan),
		ReplayTextTimeMode:       "⏵",
		ReplayTextPlayMode:       "⏸",
		ReplayTextCopyMode:       "COPY",
		ReplayTextVisualMode:     "VISUAL",
		ReplayTextVisualLineMode: "VISUAL LINE",
		SearchStatusBarStyle:     style.NewStyle(style.White, style.Blue),
		SearchTextSearching:      "searching",
		SearchTextNoMatchesFound: "no matches found for",
		TerminalTextExited:       "exited",
	}
)
