package player

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
)

type detector struct {
	emu.Terminal
	commands []Command

	// If we have ever detected a prompt
	havePrompt bool

	from      geom.Vec2
	fromWrite emu.WriteID
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

	fromWrite := d.fromWrite
	d.fromWrite = toWrite

	// We do nothing on the first prompt, just make a note of it
	if !d.havePrompt {
		d.havePrompt = true
		return
	}

	command, ok := d.getCommand(from, to, fromWrite, toWrite)
	if !ok {
		return
	}

	d.commands = append(d.commands, command)
}
