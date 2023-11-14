package emu

import (
	"github.com/cfoust/cy/pkg/geom"
)

type Scroll struct {
	Up            bool
	Origin, Count int
}

type Dirty struct {
	Lines map[int]bool // line dirtiness
	// the most recent cell that was `setChar`'d
	Print   Cell
	Printed bool

	Scroll   Scroll
	Scrolled bool

	Clear   geom.Rect
	Cleared bool

	Flag ChangeFlag
}

func (t *State) Changes() *Dirty {
	return t.dirty
}

func (d *Dirty) markScreen() {
	d.Flag |= ChangedScreen
}

func (t *State) markDirtyLine(row int) {
	index := clamp(row, 0, t.rows-1)

	if _, ok := t.dirty.Lines[index]; ok {
		return
	}

	t.dirty.Lines[index] = true
}

// Reset the change mask and dirtiness.
func (d *Dirty) Reset() {
	d.Lines = make(map[int]bool)
	d.Flag = 0
	d.Printed = false
	d.Scrolled = false
	d.Cleared = false
}

func (d *Dirty) ScreenChanged() bool {
	return d.Flag&ChangedScreen != 0
}
