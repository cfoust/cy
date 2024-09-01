package search

import (
	"fmt"

	"github.com/cfoust/cy/pkg/geom/tty"
)

func (s *Search) View(state *tty.State) {
	//size := state.Image.Size()

	if s.searching {
		numFiles := len(s.pending)
		numComplete := 0
		for _, result := range s.pending {
			if result.Done {
				numComplete++
			}
		}

		s.render.RenderAt(
			state.Image,
			0, 0,
			fmt.Sprintf("%d/%d", numComplete, numFiles),
		)
	}

	if s.replay == nil {
		return
	}

	tty.Copy(
		s.inner.Position,
		state,
		s.replay.State(),
	)
}
