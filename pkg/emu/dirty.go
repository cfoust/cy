package emu

func (t *State) markDirtyLine(row int) {
	index := clamp(row, 0, t.rows-1)

	if _, ok := t.dirty[index]; ok {
		return
	}

	t.dirty[index] = true
}

func (t *State) LastCell() (cell Cell, changed bool) {
	cell = t.lastCell
	changed = t.lastCell.changed
	t.lastCell.changed = false
	return
}

// resetChanges resets the change mask and dirtiness.
func (t *State) resetChanges() {
	t.dirty = make(map[int]bool)
	t.changed = 0
}
