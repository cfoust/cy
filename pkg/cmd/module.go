package cmd

import (
	"time"

	"github.com/cfoust/cy/pkg/replay/detect"
)

// CommandEvent is published when a command running in a pane completes
// execution.
type CommandEvent struct {
	detect.Command
	Timestamp time.Time
	Borg      string
	Cwd       string
}
