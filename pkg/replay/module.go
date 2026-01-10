package replay

import (
	"context"
	"time"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/params"
	"github.com/cfoust/cy/pkg/replay/motion"
	"github.com/cfoust/cy/pkg/replay/movement"
	"github.com/cfoust/cy/pkg/replay/player"
	"github.com/cfoust/cy/pkg/sessions/search"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
)

type Replay struct {
	util.Lifetime
	*player.Player
	params *params.Parameters

	render               *taro.Renderer
	timeBinds, copyBinds *bind.Engine[bind.Action]

	// Whether to show segmented command input and output. Only useful in
	// testing (for now?)
	showCommands bool

	// whether Replay will actually quit itself
	preventExit bool

	// Run on Init(). Default is to seek to the end.
	initialCmd tea.Cmd

	// Options cannot be applied until after the initial seek is complete.
	postSeekOptions []Option

	// Highlights provided on creation with WithHighlights
	providedHighlights []movement.Highlight

	// whether the player is seeking
	isSeeking bool
	// seeking progress is not shown until it's taken longer than 120ms
	showSeek  bool
	seekState *seekState
	// seekDelay is used in stories to simulate long operations.
	seekDelay time.Duration
	// The background image used when seeking from the beginning
	bg image.Image

	// the size of the client, but minus one row
	// we don't want to obscure content
	viewport geom.Size

	mode Mode

	// Replay allows you to browse the contents of the terminal screen in
	// two different ways:
	//   - By treating the screen as an image with arbitrary content that
	//   may not take the form of lines, such as when a full-screen
	//   application is open
	//   - By treating the screen as a text file with human-readable text,
	//   such as when a shell is running
	isSwapped bool

	isPlaying    bool
	playbackRate int
	currentTime  time.Time

	movement movement.Movement

	// Whether moving in time should skip inactivity
	skipInactivity bool

	// Whether the user has started selecting.
	isSelecting bool
	// The location in terminal space where the select began
	selectStart geom.Vec2

	isForward bool
	isWaiting bool
	// Whether no matches came back
	isEmpty         bool
	searchProgress  chan int
	progressPercent int
	input           textinput.Model
	matches         []search.SearchResult

	incr *motion.Incremental

	// The last character the user jumped to
	jumpChar string
	// Whether that jump was forward
	wasJumpForward bool
	// Whether that jump was "to" or up until
	wasJumpTo bool

	// borgPath is the path of the on-disk recording to load on demand.
	borgPath string
	// borgFlush flushes the on-disk recording before loading it, if set.
	borgFlush func(context.Context) error
	// Whether the replay history has been loaded from disk.
	historyLoaded bool
	// loadingHistory indicates a background disk load is in progress.
	loadingHistory bool
	// pendingMsg is replayed after history is loaded.
	pendingMsg tea.Msg
	// loadErr is set if loading the recording failed.
	loadErr error
	// snapshot is the state shown before history is loaded.
	snapshot *tty.State
	// loadRestore captures view state that should be restored after history is
	// loaded from disk.
	loadRestore *loadRestoreState
}

var _ taro.Model = (*Replay)(nil)

func (r *Replay) isCopyMode() bool {
	return r.mode == ModeCopy
}

func (r *Replay) enterCopyMode() {
	r.mode = ModeCopy
	r.movement.Snap()
}

func (r *Replay) swapScreen() {
	if !r.IsAltMode() {
		return
	}

	r.mode = ModeCopy
	r.isSwapped = !r.isSwapped
	r.isSelecting = false
	r.initializeMovement()
}

func (r *Replay) Init() tea.Cmd {
	return tea.Batch(
		textinput.Blink,
		r.initialCmd,
	)
}

func newReplay(
	ctx context.Context,
	player *player.Player,
	timeBinds, copyBinds *bind.Engine[bind.Action],
	options ...Option,
) *Replay {
	input := textinput.New()
	input.Focus()
	input.CharLimit = 0
	input.Width = 20
	input.Prompt = ""

	r := &Replay{
		Lifetime:       util.NewLifetime(ctx),
		incr:           motion.NewIncremental(),
		Player:         player,
		render:         taro.NewRenderer(),
		input:          input,
		playbackRate:   1,
		timeBinds:      timeBinds,
		copyBinds:      copyBinds,
		searchProgress: make(chan int),
		skipInactivity: true,
		params:         params.New(),
		historyLoaded:  true,
	}

	for _, option := range options {
		option(r)
	}

	if r.initialCmd == nil {
		r.initialCmd = r.gotoIndex(-1, -1)
	}

	return r
}

type Option func(r *Replay)

// WithNoQuit makes it so Replay will never exit.
func WithNoQuit(r *Replay) {
	r.preventExit = true
}

// WithCopyMode puts Replay immediately into copy mode.
func WithCopyMode(r *Replay) {
	// Always enter copy mode immediately when possible, but also reapply after
	// any initial seek completes (since seeking resets the view state).
	r.mode = ModeCopy
	if r.movement != nil {
		r.movement.Snap()
	}
	r.postSeekOptions = append(r.postSeekOptions, func(r *Replay) {
		r.enterCopyMode()
	})
}

// WithHighlights provides highlights to apply to the screen on creation.
func WithHighlights(highlights []movement.Highlight) Option {
	return func(r *Replay) {
		r.providedHighlights = highlights
	}
}

// WithFlow swaps to flow mode, if possible.
func WithFlow(r *Replay) {
	r.postSeekOptions = append(r.postSeekOptions,
		func(r *Replay) {
			if r.isFlowMode() {
				return
			}

			r.swapScreen()
		},
	)
}

// WithResults provides existing search results to the Replay.
func WithResults(results []search.SearchResult) Option {
	return func(r *Replay) {
		r.isWaiting = true
		r.initialCmd = r.handleSearchResult(SearchResultEvent{
			Forward: true,
			Results: results,
		})
	}
}

// WithLocation attempts to move the cursor to `location`, which is a point in
// the reference frame of the Movement.
func WithLocation(location geom.Vec2) Option {
	return func(r *Replay) {
		r.postSeekOptions = append(r.postSeekOptions,
			func(r *Replay) {
				r.enterCopyMode()
				r.movement.Goto(location)
			},
		)
	}
}

// WithParams provides parameters this Replay will use for rendering.
func WithParams(params *params.Parameters) Option {
	return func(r *Replay) {
		r.params = params
	}
}

// WithBorgPath enables on-demand loading of replay history from a .borg file.
func WithBorgPath(path string) Option {
	return func(r *Replay) {
		r.borgPath = path
		r.historyLoaded = false
		// Avoid an initial seek; it would require history to be loaded.
		r.initialCmd = func() tea.Msg { return nil }
	}
}

// WithBorgFlush sets a callback that is invoked before loading history from the
// .borg file. This is used to ensure any buffered writes are flushed to disk
// so the loader sees the latest output.
func WithBorgFlush(flush func(context.Context) error) Option {
	return func(r *Replay) {
		r.borgFlush = flush
	}
}

// WithSnapshot provides the state to show before history is loaded.
func WithSnapshot(state *tty.State) Option {
	return func(r *Replay) {
		r.snapshot = state
	}
}

// pollBinds subscribes to BindEvents from a binding engine and forwards them
// to the Replay program so that it can decide whether to emit them (after
// which they will be executed by cy).
func pollBinds(
	ctx context.Context,
	program *taro.Program,
	engine *bind.Engine[bind.Action],
) {
	for {
		select {
		case <-ctx.Done():
			return
		case event := <-engine.Recv():
			if bindEvent, ok := event.(bind.BindEvent); ok {
				program.Send(bindEvent)
			}
		}
	}
}

func New(
	ctx context.Context,
	player *player.Player,
	timeBinds, copyBinds *bind.BindScope,
	options ...Option,
) *taro.Program {
	replayEngine := bind.Run(ctx, timeBinds)
	copyEngine := bind.Run(ctx, copyBinds)
	r := newReplay(
		ctx,
		player,
		replayEngine,
		copyEngine,
		options...,
	)
	program := taro.New(ctx, r, taro.WithKittyKeys)

	go pollBinds(ctx, program, replayEngine)
	go pollBinds(ctx, program, copyEngine)

	return program
}
