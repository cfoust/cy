package search

import (
	"github.com/cfoust/cy/pkg/geom/tty"
)

func (s *Search) View(state *tty.State) {
	if s.replay == nil {
		return
	}

	tty.Copy(
		s.inner.Position,
		state,
		s.replay.State(),
	)
}
