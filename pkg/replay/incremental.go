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
		switch msg.Type() {
		case taro.KeyEsc, taro.KeyCtrlC:
			r.incr.Cancel(r.movement)
			return r, nil
		case taro.KeyEnter:
			r.incr.Accept()
			return r, nil
		}
	}

	var cmd tea.Cmd
	inputMsg := msg
	if key, ok := msg.(taro.KeyMsg); ok {
		inputMsg = key.ToTea()
	}
	r.input, cmd = r.input.Update(inputMsg)
	r.incr.Pattern(r.movement, r.input.Value())
	return r, cmd
}
