package replay

import (
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/replay/detect"
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

func (r *Replay) nextCommand(
	isForward bool,
	target func(detect.Command) geom.Vec2,
) (command detect.Command, ok bool) {
	commands := r.Commands()
	if len(commands) == 0 {
		return
	}

	cursor := r.movement.Cursor()

	if isForward {
		for _, command := range commands {
			if target(command).GT(cursor) {
				return command, true
			}
		}
	} else {
		for i := len(commands) - 1; i >= 0; i-- {
			command := commands[i]
			if target(command).LT(cursor) {
				return command, true
			}
		}
	}

	return
}

func (r *Replay) jumpCommand(isForward bool) (taro.Model, tea.Cmd) {
	if !r.isFlowMode() {
		return r, nil
	}

	command, ok := r.nextCommand(
		isForward,
		func(command detect.Command) geom.Vec2 {
			return command.InputStart()
		},
	)
	if !ok {
		return r, nil
	}
	r.movement.Goto(command.InputStart())
	return r, nil
}

func (r *Replay) jumpSelectCommand(isForward bool) (taro.Model, tea.Cmd) {
	if !r.isFlowMode() {
		return r, nil
	}

	command, ok := r.nextCommand(
		isForward,
		func(command detect.Command) geom.Vec2 {
			return command.Output.From
		},
	)
	if !ok {
		return r, nil
	}
	r.selection = SelectChar
	r.selectStart = command.Output.To
	r.movement.Goto(command.Output.From)
	return r, nil
}
