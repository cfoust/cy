package replay

import (
	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
)

func (r *Replay) jumpCommandTime(isForward bool) (taro.Model, tea.Cmd) {
	commands := r.Commands()
	if len(commands) == 0 {
		return r, nil
	}

	location := r.Location()
	index := -1

	if isForward {
		for i, command := range commands {
			if location.Index < command.Executed {
				index = i
				break
			}
		}
	} else {
		for i := len(commands) - 1; i >= 0; i-- {
			command := commands[i]
			if location.Index > command.Executed {
				index = i
				break
			}
		}

	}

	if index == -1 {
		return r, nil
	}

	return r, r.gotoIndex(commands[index].Executed, -1)
}

func (r *Replay) jumpCommand(isForward bool) (taro.Model, tea.Cmd) {
	commands := r.Commands()
	if !r.isFlowMode() || len(commands) == 0 {
		return r, nil
	}

	cursor := r.movement.Cursor()
	index := -1

	if isForward {
		for i, command := range commands {
			if command.InputStart().GT(cursor) {
				index = i
				break
			}
		}
	} else {
		for i := len(commands) - 1; i >= 0; i-- {
			command := commands[i]
			if command.InputStart().LT(cursor) {
				index = i
				break
			}
		}
	}

	if index == -1 {
		return r, nil
	}

	r.movement.Goto(commands[index].InputStart())
	return r, nil
}
