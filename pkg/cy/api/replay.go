package api

import (
	_ "embed"
	"fmt"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/input/fuzzy/preview"
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/replay"
	"github.com/cfoust/cy/pkg/replay/loader"
	"github.com/cfoust/cy/pkg/replay/replayable"
	"github.com/cfoust/cy/pkg/sessions/search"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"
)

type ReplayModule struct {
	Lifetime             util.Lifetime
	Tree                 *tree.Tree
	TimeBinds, CopyBinds *bind.BindScope
}

func (m *ReplayModule) send(context interface{}, msg taro.Msg) error {
	client, err := getClient(context)
	if err != nil {
		return err
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

func (m *ReplayModule) SwapScreen(context interface{}) error {
	return m.sendAction(context, replay.ActionSwapScreen)
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

func (m *ReplayModule) Copy(context interface{}, register string) error {
	return m.send(context, replay.CopyEvent{
		Register: register,
	})
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

func (m *ReplayModule) CommandForward(context interface{}) error {
	return m.sendAction(context, replay.ActionCommandForward)
}

func (m *ReplayModule) CommandBackward(context interface{}) error {
	return m.sendAction(context, replay.ActionCommandBackward)
}

func (m *ReplayModule) CommandSelectForward(context interface{}) error {
	return m.sendAction(context, replay.ActionCommandSelectForward)
}

func (m *ReplayModule) CommandSelectBackward(context interface{}) error {
	return m.sendAction(context, replay.ActionCommandSelectBackward)
}

func (m *ReplayModule) StartOfLine(context interface{}) error {
	return m.sendAction(context, replay.ActionStartOfLine)
}

func (m *ReplayModule) FirstNonBlank(context interface{}) error {
	return m.sendAction(context, replay.ActionFirstNonBlank)
}

func (m *ReplayModule) EndOfLine(context interface{}) error {
	return m.sendAction(context, replay.ActionEndOfLine)
}

func (m *ReplayModule) LastNonBlank(context interface{}) error {
	return m.sendAction(context, replay.ActionLastNonBlank)
}

func (m *ReplayModule) LastNonBlankScreen(context interface{}) error {
	return m.sendAction(context, replay.ActionLastNonBlankScreen)
}

func (m *ReplayModule) FirstNonBlankScreen(context interface{}) error {
	return m.sendAction(context, replay.ActionFirstNonBlankScreen)
}

func (m *ReplayModule) StartOfScreenLine(context interface{}) error {
	return m.sendAction(context, replay.ActionStartOfScreenLine)
}

func (m *ReplayModule) MiddleOfScreenLine(context interface{}) error {
	return m.sendAction(context, replay.ActionMiddleOfScreenLine)
}

func (m *ReplayModule) MiddleOfLine(context interface{}) error {
	return m.sendAction(context, replay.ActionMiddleOfLine)
}

func (m *ReplayModule) EndOfScreenLine(context interface{}) error {
	return m.sendAction(context, replay.ActionEndOfScreenLine)
}

func (m *ReplayModule) WordForward(context interface{}) error {
	return m.sendAction(context, replay.ActionWordForward)
}

func (m *ReplayModule) WordBackward(context interface{}) error {
	return m.sendAction(context, replay.ActionWordBackward)
}

func (m *ReplayModule) WordEndForward(context interface{}) error {
	return m.sendAction(context, replay.ActionWordEndForward)
}

func (m *ReplayModule) WordEndBackward(context interface{}) error {
	return m.sendAction(context, replay.ActionWordEndBackward)
}

func (m *ReplayModule) BigWordForward(context interface{}) error {
	return m.sendAction(context, replay.ActionBigWordForward)
}

func (m *ReplayModule) BigWordBackward(context interface{}) error {
	return m.sendAction(context, replay.ActionBigWordBackward)
}

func (m *ReplayModule) BigWordEndForward(context interface{}) error {
	return m.sendAction(context, replay.ActionBigWordEndForward)
}

func (m *ReplayModule) BigWordEndBackward(context interface{}) error {
	return m.sendAction(context, replay.ActionBigWordEndBackward)
}

type OpenBorgParams struct {
	AltScreen  *bool
	Focus      *geom.Vec2
	Highlights *[]search.Selection
}

func (m *ReplayModule) OpenFile(
	groupId *janet.Value,
	path string,
	named *janet.Named[OpenBorgParams],
) (tree.NodeID, error) {
	defer groupId.Free()

	params := named.Values()

	group, err := resolveGroup(m.Tree, groupId)
	if err != nil {
		return 0, err
	}

	ctx := m.Lifetime.Ctx()
	node, create := group.NewPaneCreator(ctx)

	options := preview.ParamsToReplayOptions(
		params.AltScreen,
		params.Focus,
		params.Highlights,
	)

	options = append(options,
		replay.WithNoQuit,
		replay.WithParams(node.Params()),
	)

	replay := loader.New(
		ctx,
		node.Params(),
		m.TimeBinds,
		m.CopyBinds,
		path,
		options...,
	)

	pane := create(replay)
	return pane.Id(), nil
}

type ReplayParams struct {
	Copy      bool
	AltScreen *bool
	Focus     *geom.Vec2
}

func (m *ReplayModule) Open(
	id *janet.Value,
	named *janet.Named[ReplayParams],
) error {
	defer id.Free()

	pane, err := resolvePane(m.Tree, id)
	if err != nil {
		return err
	}

	params := named.Values()
	options := preview.ParamsToReplayOptions(
		params.AltScreen,
		params.Focus,
		nil,
	)

	options = append(options, replay.WithParams(pane.Params()))

	if params.Copy {
		options = append(options, replay.WithCopyMode)
	}

	r, ok := pane.Screen().(*replayable.Replayable)
	if !ok {
		return fmt.Errorf("node not replayable")
	}

	r.EnterReplay(options...)
	return nil
}
