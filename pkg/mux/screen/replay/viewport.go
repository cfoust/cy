package replay

import (
	"github.com/cfoust/cy/pkg/geom"
)

func (r *Replay) setViewport(newViewport geom.Size) {
	// Remove one row for our status line
	newViewport.R = geom.Max(newViewport.R-1, 0)
	r.viewport = newViewport
	r.movement.Resize(newViewport)
}

// scrollYDelta moves the viewport up and down, "dragging" the cursor along behind it.
func (r *Replay) scrollYDelta(delta int) {
	if delta == 0 {
		return
	}

	r.isPlaying = false
	r.mode = ModeCopy
	r.movement.ScrollYDelta(delta)
}

func (r *Replay) scrollXDelta(delta int) {
	if delta == 0 {
		return
	}

	r.isPlaying = false
	r.mode = ModeCopy
	r.movement.ScrollXDelta(delta)
}
