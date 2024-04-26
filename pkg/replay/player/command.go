package player

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/sessions/search"
)

type Command struct {
	// In certain circumstances, input is not contiguous
	Input []search.Selection
	// But output always is
	Output search.Selection
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

func (d *detector) getCommand(
	from, to geom.Vec2,
	fromID, toID emu.WriteID,
) (command Command, ok bool) {
	lines := d.GetLines(from.R, from.R)
	if len(lines) != 1 {
		return
	}

	// If there's nothing beyond the prompt, we ignore the command
	first := lines[0]
	if from.C+1 >= len(first) {
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
		lines = d.GetLines(row, row)
		if len(lines) != 1 {
			return
		}
		line := lines[0]
		for col := len(line) - 1; col >= 0; col-- {
			glyph := line[col]
			outputTo.R = row
			outputTo.C = col
			if glyph.Write < toID {
				break lastOutput
			}
		}
	}

	command.Output.To = outputTo

	return
}
