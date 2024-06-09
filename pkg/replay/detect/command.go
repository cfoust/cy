package detect

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/sessions/search"
)

type Command struct {
	// The human-readable representation of this command as it appeared
	// originally.
	Text string
	// In certain circumstances, input is not contiguous
	Input []search.Selection
	// But output always is
	Output search.Selection
	// Whether this command is still in progress. If true, `Output` will
	// not be valid.
	Pending bool

	// Used only to allow us to skip some work for subsequent commands,
	// since there is no direct association between WriteIDs and indices
	promptedWrite emu.WriteID

	// The indices of the event where each of these stages occurred.
	// The event at which the user was prompted
	Prompted int
	// The event at which the command was executed (it was finished being
	// input)
	Executed int
	// The event at which the command finished executing (its output ended)
	Completed int
}

func (c Command) InputStart() geom.Vec2 {
	if len(c.Input) == 0 {
		return geom.Vec2{}
	}

	return c.Input[0].From
}
