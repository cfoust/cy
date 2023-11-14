package emu

const (
	// TODO(cfoust): 11/14/23 I'm genuinely not sure why this isn't in terminfo,
	// but it is supported in the VT100 standard
	// See:
	// https://invisible-island.net/xterm/ctlseqs/ctlseqs.html
	// https://vt100.net/docs/vt510-rm/LineFeedMode.html
	LineFeedMode = "\033[20h"
)
