package replay

import (
	"context"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	R "github.com/cfoust/cy/pkg/mux/screen/replay"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/stories"
	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/xo/terminfo"
)

func createTestSession() []sessions.Event {
	return sessions.NewSimulator().
		Add(
			"\033[20h", // CRLF -- why is this everywhere?
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
	replay := R.New(ctx, events, bind.NewBindScope())

	var realMsg tea.Msg
	for _, msg := range msgs {
		realMsg = msg
		switch msg := msg.(type) {
		case R.ActionType:
			realMsg = R.ActionEvent{Type: msg}
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

var SearchTimeForward stories.InitFunc = func(ctx context.Context) mux.Screen {
	replay := createStory(
		ctx,
		createTestSession(),
		R.ActionSearchForward,
		"query",
	)

	return replay
}

var Searching stories.InitFunc = func(ctx context.Context) mux.Screen {
	replay := createStory(
		ctx,
		createTestSession(),
		R.ActionSearchForward,
		"query",
		"enter",
	)

	return replay
}

var SearchProgress stories.InitFunc = func(ctx context.Context) mux.Screen {
	replay := createStory(
		ctx,
		createTestSession(),
		R.ActionSearchForward,
		"query",
		"enter",
		R.ProgressEvent{Percent: 60},
	)

	return replay
}

var JumpForward stories.InitFunc = func(ctx context.Context) mux.Screen {
	replay := createStory(
		ctx,
		createTestSession(),
		R.ActionSearchForward,
		"3m",
	)

	return replay
}

var JumpBackward stories.InitFunc = func(ctx context.Context) mux.Screen {
	replay := createStory(
		ctx,
		createTestSession(),
		R.ActionSearchBackward,
		"3m",
	)

	return replay
}

var SearchTimeBackward stories.InitFunc = func(ctx context.Context) mux.Screen {
	replay := createStory(
		ctx,
		createTestSession(),
		R.ActionSearchBackward,
		"query",
	)

	return replay
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
}
