package replay

import (
	"github.com/cfoust/cy/pkg/geom"
)

func (r *Replay) moveCursorX(delta int) {
	if r.isImageMode() {
		return
	}

	r.mode = ModeCopy
	r.movement.MoveCursorX(delta)
}

// moveCursorDelta attempts to move the cursor relative to its current
// position. Sets `desiredCol` if the motion is horizontal (ie dx != 0).
func (r *Replay) moveCursorY(delta int) {
	// We only do a simple bounds check in image mode
	if r.isImageMode() {
		return
	}

	r.mode = ModeCopy
	r.movement.MoveCursorY(delta)
}

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

func (r *Replay) handleJump(needle string, isForward bool, isTo bool) {
	// we set these (just like vim) and go into copy mode regardless
	r.jumpChar = needle
	r.wasJumpForward = isForward
	r.wasJumpTo = isTo
	r.mode = ModeCopy
	r.movement.Jump(needle, isForward, isTo)
}
