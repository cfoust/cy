package flow

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
)

func createFlowTest(terminal emu.Terminal, size geom.Size) *flowMovement {
	movement := New(terminal, size)
	i := movement.(*flowMovement)
	return i
}
