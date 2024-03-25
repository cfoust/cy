package replay

import (
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux/screen/replay/movement"
	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
)

func (r *Replay) moveCursorX(delta int) {
	r.mode = ModeCopy
	r.movement.MoveCursorX(delta)
}

// moveCursorDelta attempts to move the cursor relative to its current
// position. Sets `desiredCol` if the motion is horizontal (ie dx != 0).
func (r *Replay) moveCursorY(delta int) {
	r.mode = ModeCopy
	r.movement.MoveCursorY(delta)
}

func (r *Replay) resize(newViewport geom.Size) {
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

func (r *Replay) handleCopy() (taro.Model, tea.Cmd) {
	if !r.isCopyMode() || !r.isSelecting {
		return r, nil
	}

	r.isSelecting = false
	text := r.movement.ReadString(r.selectStart, r.movement.Cursor())
	return r, func() tea.Msg {
		return taro.PublishMsg{
			Msg: CopyEvent{
				Text: text,
			},
		}
	}
}

func (r *Replay) initializeMovement() {
	r.movement = movement.NewImage(r.Terminal)
	if !r.isImageMode() {
		r.movement = movement.NewFlow(r.Terminal)
	}
	r.movement.Resize(r.viewport)
}

func (r *Replay) exitCopyMode() {
	r.mode = ModeTime
	r.isSelecting = false
	r.initializeMovement()
}
