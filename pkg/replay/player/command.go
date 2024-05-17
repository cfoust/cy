package player

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
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

func (d *detector) getLine(row int) (line emu.Line, ok bool) {
	lines := d.GetLines(row, row)
	if len(lines) != 1 {
		return
	}

	return lines[0], true
}

// getCommand detects a command's prompt, input, and output.
func (d *detector) getCommand(
	from, to geom.Vec2,
	toID emu.WriteID,
) (command Command, ok bool) {
	// If there's nothing beyond the prompt, we ignore the command
	first, lineOk := d.getLine(from.R)
	if !lineOk || from.C+1 >= len(first) {
		return
	}

	ok = true

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
		line, lineOk := d.getLine(row)
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

	first, lineOk = d.getLine(outputFrom.R)

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
		line, lineOk := d.getLine(row)
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

func (d *detector) getInputText(command Command) (text string, ok bool) {
	numInput := len(command.Input)
	for i, input := range command.Input {
		line, lineOk := d.getLine(input.From.R)
		if !lineOk {
			return
		}

		text += line[input.From.C:input.To.C].String()

		if i < numInput-1 {
			text += "\n"
		}
	}

	ok = true
	return
}

// getPending detects the input for a command that has not finished executing.
func (d *detector) getPending(from geom.Vec2) (command Command, ok bool) {
	if !d.havePrompt {
		return
	}

	flow := d.Flow(d.Size(), d.Root())
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
	command, ok = d.getCommand(from, to, toID+1)
	if !ok {
		return
	}

	command.Pending = true
	command.Output.To = geom.Vec2{
		R: to.R,
		C: to.C + 1,
	}

	text, textOk := d.getInputText(command)
	if !textOk {
		ok = false
		return
	}

	command.Text = text

	return
}
