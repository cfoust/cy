package player

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/sessions/search"
)

type Command struct {
	Prompt search.Selection
	// In certain circumstances, input is not contiguous
	Input []search.Selection
	// But output always is
	Output search.Selection
}

type detector struct {
	terminal emu.Terminal
}
