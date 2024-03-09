package replay

import (
	"testing"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/sessions"
)

func TestFlowLines(t *testing.T) {
	s := sessions.NewSimulator().
		Add(
			geom.Size{R: 2, C: 4},
			emu.LineFeedMode,
			"foo\n",
			"foobar\n",
			"baz",
			// should be:
			// 0: foo
			// 1: foob
			// ---
			// 1: ar
			// 2: baz
		)

	r, i := createTest(s.Events())
	i(geom.Size{R: 3, C: 4})
	lines, ok := r.getFlowLines(geom.Vec2{R: 1}, -2)
	t.Logf("%+v %+v", lines, ok)
	t.Fail()
}
