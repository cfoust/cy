package detect

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/sessions"

	"github.com/sasha-s/go-deadlock"
)

const (
	CY_HOOK     = "cy"
	TEST_PROMPT = "\033Pcy\033\\$ "
)

type Detector struct {
	mu       deadlock.RWMutex
	commands []Command
	// If we have ever detected a prompt
	havePrompt bool
	from       geom.Vec2
	fromID     emu.WriteID
	handler    CommandHandler

	// The directory as provided by OSC-7 at the last prompt
	lastDir string
}

func (d *Detector) getLine(
	term emu.Terminal,
	row int,
) (line emu.Line, ok bool) {
	lines := term.GetLines(row, row)
	if len(lines) != 1 {
		return
	}

	return lines[0], true
}

func (d *Detector) getWrite(
	term emu.Terminal,
	loc geom.Vec2,
) (id emu.WriteID, ok bool) {
	var line emu.Line
	line, ok = d.getLine(term, loc.R)
	if !ok {
		return
	}

	if loc.C < 0 || loc.C >= len(line) {
		return
	}

	return line[loc.C].Write, true
}

func (d *Detector) getIndex(
	events []sessions.Event,
	id emu.WriteID,
) (index int, ok bool) {
	var write emu.WriteID

	// Reuse the translation from the previous command
	if len(d.commands) > 0 {
		command := d.commands[len(d.commands)-1]
		write = command.promptedWrite
		index = command.Prompted + 1
	}

	for i := index; i < len(events); i++ {
		if _, ok := events[i].Message.(P.OutputMessage); !ok {
			continue
		}

		write++

		if id == write {
			return i, true
		}
	}

	return
}

func (d *Detector) Commands(
	term emu.Terminal,
	events []sessions.Event,
) []Command {
	d.mu.RLock()
	var (
		complete  = d.commands
		from      = d.from
		fromWrite = d.fromID
	)
	d.mu.RUnlock()

	commands := make([]Command, len(complete))
	copy(commands, complete)

	pending, ok := d.detectPending(term, events, from, fromWrite)
	if ok {
		commands = append(commands, pending)
	}

	return commands
}

type Option func(*Detector)

func WithHandler(c CommandHandler) Option {
	return func(d *Detector) {
		d.handler = c
	}
}

func New(options ...Option) *Detector {
	d := &Detector{}

	for _, opt := range options {
		opt(d)
	}

	return d
}
