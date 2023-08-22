//go:build linux || darwin || dragonfly || solaris || openbsd || netbsd || freebsd
// +build linux darwin dragonfly solaris openbsd netbsd freebsd

package emu

import (
	"bufio"
	"unicode/utf8"
)

type terminal struct {
	*State
}

func newTerminal(info TerminalInfo) *terminal {
	t := &terminal{newState(info.w)}
	t.init(info.cols, info.rows)
	return t
}

func (t *terminal) init(cols, rows int) {
	t.numlock = true
	t.cur.Attr.FG = DefaultFG
	t.cur.Attr.BG = DefaultBG
	t.Resize(cols, rows)
	t.reset()
}

// Write parses input and writes terminal changes to state.
func (t *terminal) Write(p []byte) (int, error) {
	t.Lock()
	var written int
	for _, b := range p {
		t.parser.Advance(b)
		written++
	}
	t.Unlock()

	return written, nil
}

func fullRuneBuffered(br *bufio.Reader) bool {
	n := br.Buffered()
	buf, err := br.Peek(n)
	if err != nil {
		return false
	}
	return utf8.FullRune(buf)
}

func (t *terminal) Resize(cols, rows int) {
	t.Lock()
	defer t.Unlock()
	_ = t.resize(cols, rows)
}
