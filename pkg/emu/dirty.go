package emu

import (
	"github.com/cfoust/cy/pkg/geom"
)

// SemanticPromptType represents the type of OSC 133 semantic prompt marker.
type SemanticPromptType int

const (
	// PromptStart indicates OSC 133 ; A - prompt started
	PromptStart SemanticPromptType = iota
	// CommandStart indicates OSC 133 ; B - command input started
	CommandStart
	// CommandExecuted indicates OSC 133 ; C - command executed
	CommandExecuted
	// CommandFinished indicates OSC 133 ; D - command finished with exit code
	CommandFinished
)

// SemanticPromptEvent represents an OSC 133 semantic prompt event.
type SemanticPromptEvent struct {
	Type     SemanticPromptType
	WriteID  WriteID
	ExitCode *int // Only set for CommandFinished (D marker)
}

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

	// SemanticPrompts contains OSC 133 semantic prompt events since the
	// last Reset().
	SemanticPrompts []SemanticPromptEvent
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

// AddSemanticPrompt records an OSC 133 semantic prompt event.
func (d *Dirty) AddSemanticPrompt(
	promptType SemanticPromptType,
	writeID WriteID,
	exitCode *int,
) {
	d.SemanticPrompts = append(d.SemanticPrompts, SemanticPromptEvent{
		Type:     promptType,
		WriteID:  writeID,
		ExitCode: exitCode,
	})
}

// HasSemanticPrompt returns true if any OSC 133 events have been recorded
// since the last Reset().
func (d *Dirty) HasSemanticPrompt() bool {
	return len(d.SemanticPrompts) > 0
}

// GetSemanticPrompts returns all OSC 133 events recorded since the last
// Reset().
func (d *Dirty) GetSemanticPrompts() []SemanticPromptEvent {
	return d.SemanticPrompts
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

	d.SemanticPrompts = d.SemanticPrompts[:0]
}

func (d *Dirty) ScreenChanged() bool {
	return d.Flag&ChangedScreen != 0
}
