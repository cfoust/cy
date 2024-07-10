package params

//go:generate python3 defaults.py
type defaultParams struct {
	// Whether to enable animation.
	// boolean, default: true
	Animate bool
	// The directory in which .borg files will be saved.
	// string, default: inferred from $XDG_DATA_HOME
	DataDirectory string
	// The default shell with which to start panes.
	// string, default: /bin/bash, but also $SHELL
	DefaultShell string
}

var (
	defaults = defaultParams{
		Animate:       true,
		DataDirectory: "",
		DefaultShell:  "/bin/bash",
	}
)
