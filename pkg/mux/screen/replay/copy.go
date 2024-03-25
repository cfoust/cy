package replay

import (
	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
)

func (r *Replay) handleCopy() (taro.Model, tea.Cmd) {
	if !r.isCopyMode() || !r.isSelecting {
		return r, nil
	}

	r.isSelecting = false
	text := r.movement.ReadString(r.selectStart, r.movement.Cursor())
	return r, func() tea.Msg {
		return taro.PublishMsg{
			Msg: CopyEvent{
				Text: text,
			},
		}
	}
}
