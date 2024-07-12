package params

//go:generate python3 defaults.py
type defaultParams struct {
	// Whether to enable animation.
	Animate bool
	// The directory in which .borg files will be saved.
	// string, default: inferred from $XDG_DATA_HOME
	DataDirectory string
	// The default shell with which to start panes.
	// string, default: /bin/bash, but also $SHELL
	DefaultShell string
	// The frame used for all new clients. A blank string means a random
	// frame will be chosen from all frames.
	DefaultFrame string
	// @hide
	// Whether to avoid blocking on (input/*) calls. Just for testing.
	SkipInput bool
}

var (
	defaults = defaultParams{
		Animate:       true,
		DataDirectory: "",
		DefaultFrame:  "",
		DefaultShell:  "/bin/bash",
		SkipInput:     false,
	}
)
