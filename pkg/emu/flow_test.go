package emu

import (
	"testing"

	"github.com/cfoust/cy/pkg/geom"
)

func TestFlowLines(t *testing.T) {
	term := New()
	term.Resize(2, 4)
	term.Write([]byte(LineFeedMode))
	term.Write([]byte("foo\nfoobar\nbaz"))
	// should be:
	// 0: foo
	// 1: foob
	// ---
	// 1: ar
	// 2: baz

	lines, ok := term.Flow(
		geom.Vec2{R: 2, C: 4},
		geom.Vec2{R: 1},
		-2,
	)
	t.Logf("%+v %+v", lines, ok)
	t.Fail()
}
