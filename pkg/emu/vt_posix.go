//go:build linux || darwin || dragonfly || solaris || openbsd || netbsd || freebsd
// +build linux darwin dragonfly solaris openbsd netbsd freebsd

package emu

import (
	"github.com/cfoust/cy/pkg/geom"
)

type terminal struct {
	*State
}

func newTerminal(info TerminalInfo) *terminal {
	t := &terminal{newState(info.w)}
	t.init(geom.Size{C: info.cols, R: info.rows})
	t.disableHistory = info.disableHistory
	t.disableWrap = info.disableWrap
	return t
}

func (t *terminal) init(size geom.Size) {
	t.cur.Attr.FG = DefaultFG
	t.cur.Attr.BG = DefaultBG
	t.Resize(size)
	t.reset()
}

// Write parses input and writes terminal changes to state.
func (t *terminal) Parse(p []byte) (written int) {
	t.dirty.writeId++

	for _, b := range p {
		t.parser.Advance(b)
		written++
	}
	return
}

func (t *terminal) Write(p []byte) (int, error) {
	t.Lock()
	w := t.Parse(p)
	t.Unlock()
	return w, nil
}

func (t *terminal) Resize(size geom.Vec2) {
	t.Lock()
	defer t.Unlock()
	t.resize(size)
}
