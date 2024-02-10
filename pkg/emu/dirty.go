package emu

import (
	"github.com/cfoust/cy/pkg/geom"
)

type Scroll struct {
	Up            bool
	Origin, Count int
}

type Dirty struct {
	// The ID of the most recent call to Write().
	writeId WriteID

	// A mapping from the hook -> whether it has appeared since the last
	// Reset()
	hooks     map[string]bool
	hookState []byte
	// The number of bytes used in hookState so we can avoid allocations
	hookCount int

	// line dirtiness
	Lines map[int]bool

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

// SetHooks registers strings to be compared against any Device Control String
// input. See https://vt100.net/docs/vt510-rm/chapter4.html for more
// information.
//
// An example of a device control string is "\033Pcy\033". Specifying "cy" in a
// call to `SetHooks` means that if this specific byte sequence appears in the
// input to the terminal since the last Reset(), Hook("cy") will return true.
func (d *Dirty) SetHooks(hooks []string) {
	d.hooks = make(map[string]bool)
	for _, hook := range hooks {
		d.hooks[hook] = false
	}
}

// Hook returns true if the hook has appeared since the last Reset().
func (d *Dirty) Hook(hook string) (value, ok bool) {
	value, ok = d.hooks[hook]
	return
}

func (d *Dirty) LastWrite() WriteID {
	return d.writeId
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

	d.hookCount = 0
	for hook := range d.hooks {
		d.hooks[hook] = false
	}
}

func (d *Dirty) ScreenChanged() bool {
	return d.Flag&ChangedScreen != 0
}
