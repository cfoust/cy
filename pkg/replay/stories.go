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

func createStory(
	ctx context.Context,
	events []sessions.Event,
	msgs ...any,
) (*Replay, func() mux.Screen) {
	replayEngine := bind.Run(ctx, bind.NewBindScope())
	copyEngine := bind.Run(ctx, bind.NewBindScope())
	r := newReplay(
		ctx,
		player.FromEvents(events),
		replayEngine,
		copyEngine,
	)

	r.searchProgress = nil

	test := taro.Test(r, taro.WithKittyKeys)
	test(tea.WindowSizeMsg{
		Width:  geom.DEFAULT_SIZE.C,
		Height: geom.DEFAULT_SIZE.R,
	})

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
		test(realMsg)
	}

	r.searchProgress = make(chan int)

	return r, func() mux.Screen {
		program := taro.Existing(ctx, r)
		go pollBinds(ctx, program, replayEngine)
		go pollBinds(ctx, program, copyEngine)
		return program
	}

}

func createTrivialStory(msgs ...any) stories.InitFunc {
	return func(ctx context.Context) (mux.Screen, error) {
		_, run := createStory(
			ctx,
			createStorySession(),
			msgs...,
		)

		return run(), nil
	}
}

var SearchTimeForward stories.InitFunc = createTrivialStory(
	ActionSearchForward,
	"query",
)

var Searching stories.InitFunc = createTrivialStory(
	ActionSearchForward,
	"query",
	"enter",
)

var SearchProgress stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	r, run := createStory(
		ctx,
		createStorySession(),
		ActionSearchForward,
		"query",
		"enter",
	)

	r.isWaiting = true
	r.progressPercent = 50

	return run(), nil
}

var JumpForward stories.InitFunc = createTrivialStory(
	ActionSearchForward,
	"3m",
)

var JumpBackward stories.InitFunc = createTrivialStory(
	ActionSearchBackward,
	"3m",
)

var SearchTimeBackward stories.InitFunc = createTrivialStory(
	ActionSearchBackward,
	"query",
)

var SearchEmpty = createTrivialStory(
	ActionBeginning,
	ActionSearchForward,
	"asdf",
	"enter",
)

func createIncrementalSession() []sessions.Event {
	return sessions.NewSimulator().
		Add(
			emu.LineFeedMode,
			geom.DEFAULT_SIZE,
			`After _three thousand years_ of explosion, by means of _fragmentary_ and mechanical technologies, the Western world is imploding. During the mechanical ages we had extended our bodies in space. Today, after more than a century of electric technology, we have extended our central nervous system itself in a global embrace, abolishing both space and time as far as our planet is concerned. Rapidly, we approach the final phase of the extensions of man-- the technological simulation of consciousness, when the creative process of knowing will be collectively and corporately extended to the whole of human society, much as we have already extended our senses and our nerves by the various media Whether the extension of consciousness, so long sought by advertisers for specific products, will be "a good thing" is a question that admits of a wide solution. There is little possibility of answering such questions about the extensions of man without considering all of them together. Any extension, whether of skin, hand, or foot, affects the whole psychic and social complex.
`,
		).
		Events()
}

var Incremental stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	_, run := createStory(
		ctx,
		createIncrementalSession(),
		ActionCursorUp,
		ActionBeginning,
		ActionSearchForward,
	)

	return run(), nil
}

var IncrementalForward stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	_, run := createStory(
		ctx,
		createIncrementalSession(),
		ActionCursorUp,
		ActionBeginning,
		ActionSearchForward,
		"century",
	)

	return run(), nil
}

var IncrementalBackward stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	_, run := createStory(
		ctx,
		createIncrementalSession(),
		ActionCursorUp,
		ActionBeginning,
		ActionSearchBackward,
		"century",
	)

	return run(), nil
}

var LongHistory stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	sim := sessions.NewSimulator().
		Defaults()

	for range 100 {
		sim.Add(
			"Finally, code is a cultural resource, not trivial and only instrumental, but bound up in social change, aesthetic projects, and the relationship of people to computers. Instead of being dismissed as cryptic and irrelevant to human concerns such as art and user experience, code should be valued as text with machine and human meanings, something produced and operating within culture.\n",
		)
	}

	_, run := createStory(ctx, sim.Events())

	return run(), nil
}

func action(event ActionType) ActionEvent {
	return ActionEvent{
		Type: event,
	}
}

func init() {
	config := stories.Config{
		Size: geom.DEFAULT_SIZE,
	}
	stories.Register(
		"replay/incremental",
		Incremental,
		stories.Config{
			Size: geom.DEFAULT_SIZE,
			Input: []any{
				stories.Wait(stories.Some),
				stories.Type("century"),
				stories.Wait(stories.Some),
				stories.Type("enter"),
				stories.Wait(stories.Some),
				action(ActionSearchBackward),
				stories.Type("the"),
				stories.Wait(stories.More),
				stories.Type("enter"),
				stories.Wait(stories.More),
				action(ActionSearchAgain),
				stories.Wait(stories.More),
				action(ActionSearchAgain),
				stories.Wait(stories.More),
			},
		},
	)
	stories.Register(
		"replay/incremental/forward",
		IncrementalForward,
		config,
	)
	stories.Register(
		"replay/incremental/backward",
		IncrementalBackward,
		config,
	)
	stories.Register("replay/time/search-forward", SearchTimeForward, config)
	stories.Register("replay/time/searching", Searching, stories.Config{
		Size:       geom.DEFAULT_SIZE,
		IsSnapshot: true,
	})
	stories.Register(
		"replay/time/search-progress",
		SearchProgress,
		stories.Config{
			Size:       geom.DEFAULT_SIZE,
			IsSnapshot: true,
		},
	)
	stories.Register("replay/time/jump-forward", JumpForward, config)
	stories.Register("replay/time/search-reverse", SearchTimeBackward, config)
	stories.Register("replay/time/jump-backward", JumpBackward, config)
	stories.Register("replay/time/search-empty", SearchEmpty, config)

	stories.Register("replay/time/seek", LongHistory, stories.Config{
		Input: []any{
			//setDelayEvent{delay: 20 * time.Millisecond},
			stories.Wait(stories.Some),
			action(ActionEnd),
			stories.Wait(stories.More),
			action(ActionTimeStepBack),
			stories.Wait(stories.More),
			action(ActionTimeStepBack),
			stories.Wait(stories.More),
			action(ActionTimeStepBack),
			stories.Wait(stories.More),
			action(ActionTimeStepBack),
			stories.Wait(stories.More),
			action(ActionTimeStepBack),
			stories.Wait(stories.More),
			action(ActionTimeStepBack),
			stories.Wait(stories.More),
		},
	})
}
