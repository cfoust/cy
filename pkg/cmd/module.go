package cmd

import (
	"github.com/cfoust/cy/pkg/replay/detect"
)

// CommandEvent is published when a command running in a pane completes
// execution.
type CommandEvent struct {
	detect.Command
	Borg string
}
