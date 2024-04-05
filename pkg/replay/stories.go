package replay

import (
	"context"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/replay/player"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/stories"
	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/xo/terminfo"
)

func createStorySession() []sessions.Event {
	return sessions.NewSimulator().
		Add(
			emu.LineFeedMode,
			geom.DEFAULT_SIZE,
			"test string please ignore",
		).
		Term(terminfo.ClearScreen).
		Add("take two").
		Term(terminfo.ClearScreen).
		Add("test").
		Events()
}

func createStory(ctx context.Context, events []sessions.Event, msgs ...interface{}) mux.Screen {
	replay := New(ctx, player.FromEvents(events), bind.NewBindScope(nil))

	var realMsg tea.Msg
	for _, msg := range msgs {
		realMsg = msg
		switch msg := msg.(type) {
		case ActionType:
			realMsg = ActionEvent{Type: msg}
		case string:
			keyMsgs := taro.KeysToMsg(msg)
			if len(keyMsgs) == 1 {
				realMsg = keyMsgs[0]
			}
		}
		replay.Send(realMsg)
	}

	return replay
}

var SearchTimeForward stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	replay := createStory(
		ctx,
		createStorySession(),
		ActionSearchForward,
		"query",
	)

	return replay, nil
}

var Searching stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	replay := createStory(
		ctx,
		createStorySession(),
		ActionSearchForward,
		"query",
		"enter",
	)

	return replay, nil
}

var SearchProgress stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	replay := createStory(
		ctx,
		createStorySession(),
		ActionSearchForward,
		"query",
		"enter",
		ProgressEvent{Percent: 60},
	)

	return replay, nil
}

var JumpForward stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	replay := createStory(
		ctx,
		createStorySession(),
		ActionSearchForward,
		"3m",
	)

	return replay, nil
}

var JumpBackward stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	replay := createStory(
		ctx,
		createStorySession(),
		ActionSearchBackward,
		"3m",
	)

	return replay, nil
}

var SearchTimeBackward stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	replay := createStory(
		ctx,
		createStorySession(),
		ActionSearchBackward,
		"query",
	)

	return replay, nil
}

var SearchEmpty stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	replay := createStory(
		ctx,
		createStorySession(),
		ActionBeginning,
		ActionSearchForward,
		"asdf",
		"enter",
	)

	return replay, nil
}

func init() {
	config := stories.Config{
		Size: geom.DEFAULT_SIZE,
	}
	stories.Register("replay/time/search-forward", SearchTimeForward, config)
	stories.Register("replay/time/searching", Searching, stories.Config{
		Size:       geom.DEFAULT_SIZE,
		IsSnapshot: true,
	})
	stories.Register("replay/time/search-progress", SearchProgress, stories.Config{
		Size:       geom.DEFAULT_SIZE,
		IsSnapshot: true,
	})
	stories.Register("replay/time/jump-forward", JumpForward, config)
	stories.Register("replay/time/search-reverse", SearchTimeBackward, config)
	stories.Register("replay/time/jump-backward", JumpBackward, config)
	stories.Register("replay/time/search-empty", SearchEmpty, config)
}
