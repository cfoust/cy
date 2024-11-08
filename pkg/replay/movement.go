package replay

import (
	"math/rand"

	"github.com/cfoust/cy/pkg/geom"
	I "github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/replay/motion"
	"github.com/cfoust/cy/pkg/replay/movement/flow"
	"github.com/cfoust/cy/pkg/replay/movement/image"
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

	var char rune
	r.bg = I.New(newViewport)
	for row := 0; row < newViewport.R; row++ {
		for col := 0; col < newViewport.C; col++ {
			char = 'c'
			if rand.Intn(2) == 0 {
				char = 'y'
			}
			r.bg[row][col].Char = char
		}
	}

	if r.movement == nil {
		return
	}
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
	motion.Jump(r.movement, needle, isForward, isTo)
}

func (r *Replay) handleCopy(register string) (taro.Model, tea.Cmd) {
	if !r.isCopyMode() || !r.isSelecting {
		return r, nil
	}

	r.isSelecting = false
	text := r.movement.ReadString(r.selectStart, r.movement.Cursor())
	return r, func() tea.Msg {
		return taro.PublishMsg{
			Msg: CopyEvent{
				Register: register,
				Text:     text,
			},
		}
	}
}

func (r *Replay) isFlowMode() bool {
	return !r.IsAltMode() || (r.IsAltMode() && r.isSwapped)
}

func (r *Replay) initializeMovement() {
	size := r.viewport

	// When replay is initialized, it does not have a size, which can cause
	// issues with Movement
	if size.R == 0 && size.C == 0 {
		size = geom.DEFAULT_SIZE
	}

	r.movement = image.New(r.Terminal, size)
	if r.isFlowMode() {
		r.movement = flow.New(r.Terminal, size)
	}
	r.movement.Resize(size)
}

func (r *Replay) exitCopyMode() {
	r.mode = ModeTime
	r.isSelecting = false
	r.isSwapped = false
	r.initializeMovement()
}
