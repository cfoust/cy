package emu

func (t *State) markDirtyLine(row int) {
	index := clamp(row, 0, t.rows-1)

	if _, ok := t.dirty[index]; ok {
		return
	}

	t.dirty[index] = true
	t.numDirty++
}

func (t *State) Changes() (flag ChangeFlag, dirtyRows []int) {
	t.Lock()
	for row := range t.dirty {
		dirtyRows = append(dirtyRows, row)
	}
	flag = t.changed
	t.resetChanges()
	t.Unlock()

	return
}

// resetChanges resets the change mask and dirtiness.
func (t *State) resetChanges() {
	t.dirty = make(map[int]bool)
	t.changed = 0
	t.numDirty = 0
}
