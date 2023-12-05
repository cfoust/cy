package api

import (
	_ "embed"
	"fmt"
	"io"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/mux/screen/replay"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"
)

type ReplayModule struct {
	Lifetime util.Lifetime
	Tree     *tree.Tree
	Binds    *bind.BindScope
}

func (m *ReplayModule) send(context interface{}, msg taro.Msg) error {
	client, ok := context.(Client)
	if !ok {
		return fmt.Errorf("no client could be inferred")
	}

	node := client.Node()
	if node == nil {
		return fmt.Errorf("client was missing node")
	}

	pane, ok := node.(*tree.Pane)
	if !ok {
		return fmt.Errorf("client node was not pane")
	}

	pane.Screen().Send(msg)
	return nil
}

func (m *ReplayModule) sendAction(context interface{}, action replay.ActionType) error {
	return m.send(context, replay.ActionEvent{
		Type: action,
	})
}

func (m *ReplayModule) sendArg(context interface{}, action replay.ActionType, arg string) error {
	if len(arg) != 1 {
		return nil
	}
	return m.send(context, replay.ActionEvent{
		Type: action,
		Arg:  arg,
	})
}

func (m *ReplayModule) Quit(context interface{}) error {
	return m.sendAction(context, replay.ActionQuit)
}

func (m *ReplayModule) ScrollUp(context interface{}) error {
	return m.sendAction(context, replay.ActionScrollUp)
}

func (m *ReplayModule) ScrollDown(context interface{}) error {
	return m.sendAction(context, replay.ActionScrollDown)
}

func (m *ReplayModule) HalfPageUp(context interface{}) error {
	return m.sendAction(context, replay.ActionScrollUpHalf)
}

func (m *ReplayModule) HalfPageDown(context interface{}) error {
	return m.sendAction(context, replay.ActionScrollDownHalf)
}

func (m *ReplayModule) SearchForward(context interface{}) error {
	return m.sendAction(context, replay.ActionSearchForward)
}

func (m *ReplayModule) TimePlay(context interface{}) error {
	return m.sendAction(context, replay.ActionTimePlay)
}

func (m *ReplayModule) SearchAgain(context interface{}) error {
	return m.sendAction(context, replay.ActionSearchAgain)
}

func (m *ReplayModule) SearchReverse(context interface{}) error {
	return m.sendAction(context, replay.ActionSearchReverse)
}

func (m *ReplayModule) SearchBackward(context interface{}) error {
	return m.sendAction(context, replay.ActionSearchBackward)
}

func (m *ReplayModule) TimeStepBack(context interface{}) error {
	return m.sendAction(context, replay.ActionTimeStepBack)
}

func (m *ReplayModule) TimeStepForward(context interface{}) error {
	return m.sendAction(context, replay.ActionTimeStepForward)
}

func (m *ReplayModule) Beginning(context interface{}) error {
	return m.sendAction(context, replay.ActionBeginning)
}

func (m *ReplayModule) End(context interface{}) error {
	return m.sendAction(context, replay.ActionEnd)
}

func (m *ReplayModule) CursorDown(context interface{}) error {
	return m.sendAction(context, replay.ActionCursorDown)
}

func (m *ReplayModule) CursorLeft(context interface{}) error {
	return m.sendAction(context, replay.ActionCursorLeft)
}

func (m *ReplayModule) CursorRight(context interface{}) error {
	return m.sendAction(context, replay.ActionCursorRight)
}

func (m *ReplayModule) CursorUp(context interface{}) error {
	return m.sendAction(context, replay.ActionCursorUp)
}

func (m *ReplayModule) TimePlaybackRate(context interface{}, rate int) error {
	return m.send(context, replay.PlaybackRateEvent{
		Rate: rate,
	})
}

func (m *ReplayModule) Copy(context interface{}) error {
	return m.sendAction(context, replay.ActionCopy)
}

func (m *ReplayModule) Select(context interface{}) error {
	return m.sendAction(context, replay.ActionSelect)
}

func (m *ReplayModule) JumpAgain(context interface{}) error {
	return m.sendAction(context, replay.ActionJumpAgain)
}

func (m *ReplayModule) JumpReverse(context interface{}) error {
	return m.sendAction(context, replay.ActionJumpReverse)
}

func (m *ReplayModule) JumpBackward(context interface{}, char string) error {
	return m.sendArg(context, replay.ActionJumpBackward, char)
}

func (m *ReplayModule) JumpForward(context interface{}, char string) error {
	return m.sendArg(context, replay.ActionJumpForward, char)
}

func (m *ReplayModule) JumpToForward(context interface{}, char string) error {
	return m.sendArg(context, replay.ActionJumpToForward, char)
}

func (m *ReplayModule) JumpToBackward(context interface{}, char string) error {
	return m.sendArg(context, replay.ActionJumpToBackward, char)
}

func (m *ReplayModule) Open(
	groupId tree.NodeID,
	path string,
) (tree.NodeID, error) {
	group, ok := m.Tree.GroupById(groupId)
	if !ok {
		return 0, fmt.Errorf("node not found: %d", groupId)
	}

	reader, err := sessions.Open(path)
	if err != nil {
		return 0, err
	}

	events := make([]sessions.Event, 0)
	for {
		event, err := reader.Read()
		if err == io.EOF || err == io.ErrUnexpectedEOF {
			break
		}
		if err != nil {
			return 0, err
		}
		events = append(events, event)
	}

	ctx := m.Lifetime.Ctx()
	replay := replay.New(
		ctx,
		events,
		m.Binds,
		replay.WithNoQuit,
	)

	pane := group.NewPane(ctx, replay)
	return pane.Id(), nil
}
