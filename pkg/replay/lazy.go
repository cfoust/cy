package replay

import (
	"time"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/replay/player"
	"github.com/cfoust/cy/pkg/util"

	tea "github.com/charmbracelet/bubbletea"
)

type loadDoneEvent struct {
	player *player.Player
	err    error
}

func (r *Replay) loadHistory() tea.Cmd {
	if r.isSeeking || r.historyLoaded || len(r.borgPath) == 0 {
		return nil
	}

	seekState := &seekState{
		Lifetime: util.NewLifetime(r.Ctx()),
		progress: make(chan int),
	}

	viewport := tty.New(geom.Vec2{
		R: r.viewport.R + 1,
		C: r.viewport.C,
	})
	if !r.viewport.IsZero() {
		r.View(viewport)
		seekState.screen = viewport
	}

	r.isSeeking = true
	r.showSeek = false
	r.seekState = seekState
	r.loadingHistory = true

	return tea.Batch(
		func() tea.Msg {
			p, err := player.FromBorgContext(
				seekState.Ctx(),
				r.borgPath,
				seekState.progress,
			)
			return loadDoneEvent{
				player: p,
				err:    err,
			}
		},
		r.waitSeekProgress(),
		func() tea.Msg {
			time.Sleep(SEEK_THRESHOLD)
			return seekShowEvent{}
		},
	)
}
