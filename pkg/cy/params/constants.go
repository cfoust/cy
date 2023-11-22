package params

const (
	// The directory in which .borg files will be saved.
	// string, default: inferred from $XDG_DATA_HOME
	ParamDataDirectory = "data-dir"
	// Whether to enable animation.
	// boolean, default: true
	ParamAnimate = "animate"
	// The default shell with which to start panes.
	// string, default: /bin/bash, but also $SHELL
	ParamDefaultShell = "default-shell"
)
