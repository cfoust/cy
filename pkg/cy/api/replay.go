package api

import (
	"fmt"

	"github.com/cfoust/cy/pkg/mux/screen/replay"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/taro"
)

type ReplayModule struct {
}

func (r *ReplayModule) get(context interface{}) (*taro.Program, error) {
	client, ok := context.(Client)
	if !ok {
		return nil, fmt.Errorf("no client could be inferred")
	}

	node := client.Node()
	if node == nil {
		return nil, fmt.Errorf("client was missing node")
	}

	pane, ok := node.(*tree.Pane)
	if !ok {
		return nil, fmt.Errorf("client node was not pane")
	}

	replay := pane.ReplayMode()
	if replay == nil {
		return nil, fmt.Errorf("client pane was not in replay mode")
	}

	return replay, nil
}

func (m *ReplayModule) send(context interface{}, action replay.ActionType) error {
	r, err := m.get(context)
	if err != nil {
		return err
	}

	r.Send(replay.ActionEvent{
		Type: action,
	})
	return nil
}

func (m *ReplayModule) Quit(context interface{}) error { return m.send(context, replay.ActionQuit) }

func (m *ReplayModule) ScrollUp(context interface{}) error {
	return m.send(context, replay.ActionScrollUp)
}

func (m *ReplayModule) ScrollDown(context interface{}) error {
	return m.send(context, replay.ActionScrollDown)
}

func (m *ReplayModule) HalfPageUp(context interface{}) error {
	return m.send(context, replay.ActionScrollUpHalf)
}

func (m *ReplayModule) HalfPageDown(context interface{}) error {
	return m.send(context, replay.ActionScrollDownHalf)
}

func (m *ReplayModule) SearchForward(context interface{}) error {
	return m.send(context, replay.ActionSearchForward)
}

func (m *ReplayModule) SearchAgain(context interface{}) error {
	return m.send(context, replay.ActionSearchAgain)
}

func (m *ReplayModule) SearchReverse(context interface{}) error {
	return m.send(context, replay.ActionSearchReverse)
}

func (m *ReplayModule) SearchBackward(context interface{}) error {
	return m.send(context, replay.ActionSearchBackward)
}

func (m *ReplayModule) TimeStepBack(context interface{}) error {
	return m.send(context, replay.ActionTimeStepBack)
}

func (m *ReplayModule) TimeStepForward(context interface{}) error {
	return m.send(context, replay.ActionTimeStepForward)
}

func (m *ReplayModule) Beginning(context interface{}) error {
	return m.send(context, replay.ActionBeginning)
}

func (m *ReplayModule) End(context interface{}) error {
	return m.send(context, replay.ActionEnd)
}

func (m *ReplayModule) CursorDown(context interface{}) error {
	return m.send(context, replay.ActionCursorDown)
}

func (m *ReplayModule) CursorLeft(context interface{}) error {
	return m.send(context, replay.ActionCursorLeft)
}

func (m *ReplayModule) CursorRight(context interface{}) error {
	return m.send(context, replay.ActionCursorRight)
}

func (m *ReplayModule) CursorUp(context interface{}) error {
	return m.send(context, replay.ActionCursorUp)
}
