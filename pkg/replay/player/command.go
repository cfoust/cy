package player

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/sessions/search"
)

type Command struct {
	// The human-readable representation of this command as it appeared
	// originally.
	Text string
	// In certain circumstances, input is not contiguous
	Input []search.Selection
	// But output always is
	Output search.Selection
	// Whether this command is still in progress. If true, `Output` will
	// not be valid.
	Pending bool

	// Used only to allow us to skip some work for subsequent commands,
	// since there is no direct association between WriteIDs and indices
	promptedWrite emu.WriteID

	// The indices of the event where each of these stages occurred.
	// The event at which the user was prompted
	Prompted int
	// The event at which the command was executed (it was finished being
	// input)
	Executed int
	// The event at which the command finished executing (its output ended)
	Completed int
}

type recognizer interface {
	// Decide whether the line is matched by this recognizer. If it is,
	// return the index at which the rest of the line begins.
	Check(line emu.Line) (int, bool)
}

type stringRecognizer struct{ pattern string }

var _ recognizer = (*stringRecognizer)(nil)

func staticString(pattern string) *stringRecognizer {
	return &stringRecognizer{pattern: pattern}
}

func (s *stringRecognizer) Check(line emu.Line) (start int, ok bool) {
	numChars := len(s.pattern)
	if len(line) < numChars {
		return
	}

	if line[:numChars].String() != s.pattern {
		return
	}

	ok = true
	start = numChars
	return
}

var recognizers = []recognizer{
	// bash, zsh
	staticString("> "),
	// seemingly only zsh?
	staticString("dquote> "),
	staticString("quote> "),
	// TODO(cfoust): 04/26/24 fish uses cursor manipulation to do this
}

func (p *Player) getLine(row int) (line emu.Line, ok bool) {
	lines := p.GetLines(row, row)
	if len(lines) != 1 {
		return
	}

	return lines[0], true
}

func (p *Player) getWrite(loc geom.Vec2) (id emu.WriteID, ok bool) {
	var line emu.Line
	line, ok = p.getLine(loc.R)
	if !ok {
		return
	}

	if loc.C < 0 || loc.C >= len(line) {
		return
	}

	return line[loc.C].Write, true
}

func (p *Player) getIndex(id emu.WriteID) (index int, ok bool) {
	var write emu.WriteID

	// Reuse the translation from the previous command
	if len(p.commands) > 0 {
		command := p.commands[len(p.commands)-1]
		write = command.promptedWrite
		index = command.Prompted + 1
	}

	for i := index; i < len(p.events); i++ {
		if _, ok := p.events[i].Message.(P.OutputMessage); !ok {
			continue
		}

		write++

		if id == write {
			return i, true
		}
	}

	return
}

// completeCommand fills in information about a command that's common to all
// commands, regardless of whether they've finished executing.
func (p *Player) completeCommand(command *Command) (ok bool) {
	inputs := command.Input

	var text string
	numInput := len(inputs)
	for i, input := range inputs {
		line, lineOk := p.getLine(input.From.R)
		if !lineOk {
			return
		}

		text += line[input.From.C:input.To.C].String()

		if i < numInput-1 {
			text += "\n"
		}
	}
	command.Text = text

	if len(inputs) == 0 {
		return
	}

	to := inputs[len(inputs)-1].To
	to.C--

	command.Prompted, ok = p.getIndex(command.promptedWrite)
	if !ok {
		return
	}

	var inputID emu.WriteID
	inputID, ok = p.getWrite(to)
	if !ok {
		return
	}

	command.Executed, ok = p.getIndex(inputID)
	if !ok {
		return
	}

	// If there's no output, the command.Completed might not be correct, we
	// want to bound it by command.Executed
	command.Completed = geom.Max(command.Executed, command.Completed)

	ok = true
	return
}

// getCommand detects a command's prompt, input, and output.
func (p *Player) getCommand(
	from, to geom.Vec2,
	fromID, toID emu.WriteID,
) (command Command, ok bool) {
	// If there's nothing beyond the prompt, we ignore the command
	first, lineOk := p.getLine(from.R)
	if !lineOk || from.C+1 >= len(first) {
		return
	}

	ok = true

	command.promptedWrite = fromID

	// We at least consume the first line
	command.Input = append(
		command.Input,
		search.Selection{
			From: geom.Vec2{
				R: from.R,
				C: from.C + 1,
			},
			To: geom.Vec2{
				R: from.R,
				C: len(first),
			},
		},
	)

	// Find where the output ends
	outputTo := to
lastOutput:
	for row := to.R; row > from.R; row-- {
		line, lineOk := p.getLine(row)
		if !lineOk {
			return
		}
		for col := len(line) - 1; col >= 0; col-- {
			glyph := line[col]
			outputTo.R = row
			outputTo.C = col
			if glyph.Write < toID {
				break lastOutput
			}
		}
	}

	outputFrom := geom.Vec2{
		R: from.R + 1,
		C: 0,
	}

	if outputFrom.GTE(outputTo) {
		// These are equal, so should not result in any selection
		command.Output.To = outputTo
		command.Output.From = outputTo
		return
	}

	// Selections are exclusive
	outputTo.C++
	command.Output.To = outputTo
	command.Output.From = outputFrom

	first, lineOk = p.getLine(outputFrom.R)

	// Check to see whether this is a multiline command
	// The recognizer must be consistent across all lines
	var r recognizer
	for _, other := range recognizers {
		if _, ok := other.Check(first); ok {
			r = other
			break
		}
	}

	if r == nil {
		return
	}

	for row := outputFrom.R; row <= outputTo.R; row++ {
		line, lineOk := p.getLine(row)
		if !lineOk {
			break
		}

		outputFrom.R = row
		outputFrom.C = 0

		col, isPrompt := r.Check(line)
		if !isPrompt {
			break
		}

		command.Input = append(
			command.Input,
			search.Selection{
				From: geom.Vec2{
					R: row,
					C: col,
				},
				To: geom.Vec2{
					R: row,
					C: len(line),
				},
			},
		)
	}

	command.Output.From = outputFrom

	return
}

// detectPending detects the input for a command that has not finished executing.
func (p *Player) detectPending(
	from geom.Vec2,
	fromID emu.WriteID,
) (command Command, ok bool) {
	if !p.havePrompt {
		return
	}

	command.promptedWrite = fromID

	flow := p.Flow(p.Size(), p.Root())
	if !flow.OK || !flow.CursorOK || len(flow.Lines) == 0 {
		return
	}

	lastIndex := -1
	for i := len(flow.Lines) - 1; i >= 0; i-- {
		if flow.Lines[i].Chars.Length() != 0 {
			lastIndex = i
			break
		}
	}

	if lastIndex == -1 {
		return
	}

	lastLine := flow.Lines[lastIndex]
	to := lastLine.Root()
	to.C = geom.Max(0, lastLine.C1-1)

	if len(lastLine.Chars) == 0 {
		return
	}

	// toID is usually greater than the ID of the last output cell
	toID := lastLine.Chars[len(lastLine.Chars)-1].Write
	command, ok = p.getCommand(from, to, fromID, toID+1)
	if !ok {
		return
	}

	command.Pending = true
	command.Completed = len(p.events) - 1
	command.Output.To = geom.Vec2{
		R: to.R,
		C: to.C + 1,
	}

	ok = p.completeCommand(&command)
	return
}

func (p *Player) Commands() []Command {
	p.mu.RLock()
	var (
		complete  = p.commands
		from      = p.from
		fromWrite = p.fromID
	)
	p.mu.RUnlock()

	commands := make([]Command, len(complete))
	copy(commands, complete)

	pending, ok := p.detectPending(from, fromWrite)
	if ok {
		commands = append(commands, pending)
	}

	return commands
}

func (p *Player) detect() {
	dirty := p.Changes()
	defer dirty.Reset()

	if emu.IsAltMode(p.Mode()) {
		return
	}

	if prompted, _ := dirty.Hook(CY_HOOK); !prompted {
		return
	}

	flow := p.Flow(p.Size(), p.Root())
	if !flow.OK || !flow.CursorOK {
		return
	}

	to, ok := flow.Coord(dirty.Print.Vec2)
	if !ok {
		return
	}

	toID := dirty.LastWrite()

	// If the prompt didn't produce any characters, we have no way of
	// knowing where the prompt was. This will only be the case if the most
	// recent write, the prompt, never caused a `setChar` to occur.
	if dirty.Print.Write != toID {
		return
	}

	from := p.from
	fromID := p.fromID
	p.from = to
	p.fromID = toID

	// We do nothing on the first prompt, just make a note of it
	if !p.havePrompt {
		p.havePrompt = true
		return
	}

	command, ok := p.getCommand(from, to, fromID, toID)
	if !ok {
		return
	}

	var nextPromptIndex int
	nextPromptIndex, ok = p.getIndex(toID)
	if !ok {
		return
	}

	command.Completed = nextPromptIndex - 1

	ok = p.completeCommand(&command)
	if !ok {
		return
	}

	p.commands = append(p.commands, command)
}
