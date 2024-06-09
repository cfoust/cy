package detect

import (
	"github.com/cfoust/cy/pkg/emu"
)

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
