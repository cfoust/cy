package replay

import (
	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
)

func (r *Replay) handleIncrementalInput(msg tea.Msg) (taro.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case ActionEvent:
		switch msg.Type {
		case ActionQuit:
			r.incr.Cancel(r.movement)
			return r, nil
		}
	case taro.KeyMsg:
		switch msg.Type {
		case tea.KeyEsc, tea.KeyCtrlC:
			r.incr.Cancel(r.movement)
			return r, nil
		case tea.KeyEnter:
			r.incr.Accept()
			return r, nil
		}
	}

	var cmd tea.Cmd
	r.input, cmd = r.input.Update(msg)
	r.incr.Pattern(r.movement, r.input.Value())
	return r, cmd
}
