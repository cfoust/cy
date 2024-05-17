package player

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"

	"github.com/sasha-s/go-deadlock"
)

type detector struct {
	emu.Terminal
	deadlock.RWMutex
	commands []Command
	// If we have ever detected a prompt
	havePrompt bool
	from       geom.Vec2
}

func (d *detector) Commands() []Command {
	d.RLock()
	var (
		complete = d.commands
		from     = d.from
	)
	d.RUnlock()

	commands := make([]Command, len(complete))
	copy(commands, complete)

	pending, ok := d.getPending(from)
	if ok {
		commands = append(commands, pending)
	}

	return commands
}

func (d *detector) update() {
	dirty := d.Changes()
	defer dirty.Reset()

	if emu.IsAltMode(d.Mode()) {
		return
	}

	if prompted, _ := dirty.Hook(CY_HOOK); !prompted {
		return
	}

	flow := d.Flow(d.Size(), d.Root())
	if !flow.OK || !flow.CursorOK {
		return
	}

	to, ok := flow.Coord(dirty.Print.Vec2)
	if !ok {
		return
	}

	toWrite := dirty.LastWrite()

	// If the prompt didn't produce any characters, we have no way of
	// knowing where the prompt was. This will only be the case if the most
	// recent write, the prompt, never caused a `setChar` to occur.
	if dirty.Print.Write != toWrite {
		return
	}

	from := d.from
	d.from = to

	// We do nothing on the first prompt, just make a note of it
	if !d.havePrompt {
		d.havePrompt = true
		return
	}

	command, ok := d.getCommand(from, to, toWrite)
	if !ok {
		return
	}

	text, textOk := d.getInputText(command)
	if !textOk {
		ok = false
		return
	}

	command.Text = text

	d.Lock()
	d.commands = append(d.commands, command)
	d.Unlock()
}
