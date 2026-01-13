package replay

import "github.com/cfoust/cy/pkg/geom"

type loadRestoreState struct {
	mode        Mode
	isSelecting bool
	selectStart geom.Vec2

	cursor geom.Vec2
	root   geom.Vec2
}

func (r *Replay) captureLoadRestore() {
	restore := &loadRestoreState{
		mode:        r.mode,
		isSelecting: r.isSelecting,
		selectStart: r.selectStart,
	}

	if r.movement != nil {
		restore.cursor = r.movement.Cursor()
		restore.root = restore.cursor

		lines, _, _ := r.movement.Viewport()
		if len(lines) > 0 {
			restore.root = lines[0].Root()
		}
	}

	r.loadRestore = restore
}

func (r *Replay) restoreLoadRestore() {
	restore := r.loadRestore
	r.loadRestore = nil
	if restore == nil {
		return
	}

	r.mode = restore.mode
	r.isSelecting = restore.isSelecting
	r.selectStart = restore.selectStart

	if r.movement == nil {
		return
	}

	r.movement.Goto(restore.root)
	r.movement.Goto(restore.cursor)

	if r.mode == ModeCopy {
		r.movement.Snap()
	}
}
