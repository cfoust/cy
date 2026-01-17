package emu

const (
	// TODO(cfoust): 11/14/23 I'm genuinely not sure why this isn't in terminfo,
	// but it is supported in the VT100 standard
	// See:
	// https://invisible-island.net/xterm/ctlseqs/ctlseqs.html
	// https://vt100.net/docs/vt510-rm/LineFeedMode.html
	LineFeedMode   = "\033[20h"
	EnterAltScreen = "\033[?1049h"
	ExitAltScreen  = "\033[?1049l"

	// OSC 133 semantic prompt markers (FinalTerm/iTerm2 shell integration)
	// See:
	// https://gitlab.freedesktop.org/Per_Bothner/specifications/blob/master/proposals/semantic-prompts.md
	OSC133PromptStart  = "\033]133;A\033\\"
	OSC133CommandStart = "\033]133;B\033\\"
	OSC133CommandExec  = "\033]133;C\033\\"

	// These are just for testing
	OSC133CommandDone     = "\033]133;D;0\033\\"
	OSC133CommandDone1    = "\033]133;D;1\033\\" // Exit code 1
	OSC133CommandDoneNoEC = "\033]133;D\033\\"   // No exit code
	// OSC133Prompt is a complete prompt sequence: A marker + "$ " + B marker
	OSC133Prompt = OSC133PromptStart + "$ " + OSC133CommandStart
)
