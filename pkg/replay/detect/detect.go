package detect

import (
	"strings"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/sessions/search"
)

func (d *Detector) Detect(
	term emu.Terminal,
	events []sessions.Event,
) {
	dirty := term.Changes()
	defer dirty.Reset()

	if term.IsAltMode() {
		return
	}

	for _, event := range dirty.GetSemanticPrompts() {
		switch event.Type {
		case emu.CommandStart:
			d.handlePrompt(term, events, dirty, event)
		case emu.CommandFinished:
			// Capture exit code for the next command completion
			if event.ExitCode != nil {
				d.hasLastExitCode = true
				d.lastExitCode = *event.ExitCode
			} else {
				d.hasLastExitCode = false
			}
		}
	}
}

// handlePrompt processes a detected prompt from OSC 133.
func (d *Detector) handlePrompt(
	term emu.Terminal,
	events []sessions.Event,
	dirty *emu.Dirty,
	event emu.SemanticPromptEvent,
) {
	flow := term.Flow(term.Size(), term.Root())
	if !flow.OK || !flow.CursorOK {
		return
	}

	oldDir := d.lastDir
	newDir := term.Directory()

	// Defaults to OSC-7, if available, otherwise tries to use other method
	if len(newDir) == 0 && d.getDirectory != nil {
		newDir = d.getDirectory()
	}

	d.lastDir = newDir

	to, ok := flow.Coord(dirty.Print.Vec2)
	if !ok {
		return
	}

	toID := event.WriteID

	// If the prompt didn't produce any characters, we have no way of
	// knowing where the prompt was. This will only be the case if the most
	// recent write, the prompt, never caused a `setChar` to occur.
	if dirty.Print.Write != toID {
		return
	}

	from := d.from
	fromID := d.fromID
	d.from = to
	d.fromID = toID

	// We do nothing on the first prompt, just make a note of it
	if !d.havePrompt {
		d.havePrompt = true
		return
	}

	command, ok := d.getCommand(term, from, to, fromID, toID)
	if !ok {
		return
	}

	var nextPromptIndex int
	nextPromptIndex, ok = d.getIndex(events, toID)
	if !ok {
		return
	}

	command.Directory = oldDir
	command.Completed = nextPromptIndex - 1

	// Set exit code from OSC 133 D marker if available
	if d.hasLastExitCode {
		command.HasExitCode = true
		command.ExitCode = d.lastExitCode
		d.hasLastExitCode = false
	}

	ok = d.completeCommand(term, events, &command)
	if !ok {
		return
	}

	if d.handler != nil {
		d.handler(command)
	}

	d.commands = append(d.commands, command)
}

// completeCommand fills in information about a command that's common to all
// commands, regardless of whether they've finished executing.
func (d *Detector) completeCommand(
	term emu.Terminal,
	events []sessions.Event,
	command *Command,
) (ok bool) {
	inputs := command.Input

	var text string
	numInput := len(inputs)
	for i, input := range inputs {
		line, lineOk := d.getLine(term, input.From.R)
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

	command.Prompted, ok = d.getIndex(events, command.promptedWrite)
	if !ok {
		return
	}

	var inputID emu.WriteID
	inputID, ok = d.getWrite(term, to)
	if !ok {
		return
	}

	command.Executed, ok = d.getIndex(events, inputID)
	command.ExecutedAt = events[command.Executed].Stamp
	if !ok {
		return
	}

	// If there's no output, the command.Completed might not be correct, we
	// want to bound it by command.Executed
	command.Completed = geom.Max(command.Executed, command.Completed)
	command.CompletedAt = events[command.Completed].Stamp

	ok = true
	return
}

// getCommand detects a command's prompt, input, and output.
func (d *Detector) getCommand(
	term emu.Terminal,
	from, to geom.Vec2,
	fromID, toID emu.WriteID,
) (command Command, ok bool) {
	// If there's nothing beyond the prompt, we ignore the command
	first, lineOk := d.getLine(term, from.R)
	if !lineOk || from.C+1 >= len(first) {
		return
	}

	// Ignore prompts "canceled" by pressing ctrl+c
	// TODO(cfoust): 09/15/24 handle this in multiline commands, too
	if strings.HasSuffix(first.String(), "^C") {
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
		line, lineOk := d.getLine(term, row)
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

	first, _ = d.getLine(term, outputFrom.R)

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
		line, lineOk := d.getLine(term, row)
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

// detectPending detects the input for a command that has not finished
// executing.
func (d *Detector) detectPending(
	term emu.Terminal,
	events []sessions.Event,
	from geom.Vec2,
	fromID emu.WriteID,
) (command Command, ok bool) {
	if !d.havePrompt {
		return
	}

	command.promptedWrite = fromID

	flow := term.Flow(term.Size(), term.Root())
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
	command, ok = d.getCommand(term, from, to, fromID, toID+1)
	if !ok {
		return
	}

	command.Pending = true
	command.Completed = len(events) - 1
	command.Output.To = geom.Vec2{
		R: to.R,
		C: to.C + 1,
	}

	ok = d.completeCommand(term, events, &command)
	return
}
