package replay

import (
	"context"
	"errors"
	"time"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/keys"
	"github.com/cfoust/cy/pkg/replay/motion"
	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
)

func (r *Replay) quit() (taro.Model, tea.Cmd) {
	if r.preventExit {
		return r, nil
	}

	return r, tea.Quit
}

// rootPosition captures the current position for detecting viewport movement
type rootPosition struct {
	cursor geom.Vec2
	root   geom.Vec2
}

// getRoot returns the current cursor position and the root of the first viewport line
func (r *Replay) getRoot() rootPosition {
	cursor := r.movement.Cursor()
	root := cursor
	if lines, _, _ := r.movement.Viewport(); len(lines) > 0 {
		root = lines[0].Root()
	}
	return rootPosition{cursor: cursor, root: root}
}

// checkAndLoadHistory compares before/after positions and triggers history load if unchanged
func (r *Replay) checkAndLoadHistory(
	before, after rootPosition,
	msg tea.Msg,
) (taro.Model, tea.Cmd) {
	if r.historyLoaded || len(r.borgPath) == 0 || !r.isFlowMode() {
		return r, nil
	}

	if before.cursor != after.cursor || before.root != after.root {
		return r, nil
	}

	r.pendingMsg = msg
	r.loadErr = nil
	r.captureLoadRestore()
	return r, r.loadHistory()
}

type ApplyOptionsEvent struct {
	Options []Option
}

func (r *Replay) emit(event bind.BindEvent) tea.Cmd {
	return func() taro.Msg {
		return taro.PublishMsg{
			Msg: event,
		}
	}
}

func (r *Replay) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	viewport := r.viewport

	switch msg := msg.(type) {
	case loadDoneEvent:
		r.loadingHistory = false

		// Stop showing the seek UI used for loading.
		if r.seekState != nil {
			r.seekState.Cancel()
			r.seekState = nil
		}
		r.isSeeking = false
		r.showSeek = false

		pending := r.pendingMsg
		r.pendingMsg = nil

		if msg.err != nil {
			r.loadRestore = nil

			// A cancelled load is not an error to the user.
			if errors.Is(msg.err, context.Canceled) {
				r.loadErr = nil
				return r, nil
			}

			r.loadErr = msg.err
			return r, nil
		}

		r.loadErr = nil
		if msg.player != nil {
			r.Player = msg.player
			r.historyLoaded = true
			r.snapshot = nil

			r.initializeMovement()
			r.restoreLoadRestore()

			events := r.Events()
			index := r.Location().Index
			if index >= 0 && index < len(events) {
				r.currentTime = events[index].Stamp
			}

			if r.postSeekOptions != nil {
				for _, option := range r.postSeekOptions {
					option(r)
				}
				r.postSeekOptions = nil
			}
		}

		if pending == nil {
			return r, nil
		}

		// Re-run whatever user input triggered loading.
		return r, func() tea.Msg { return pending }
	case ApplyOptionsEvent:
		for _, option := range msg.Options {
			option(r)
		}
		return r, nil
	case seekProgressEvent:
		if r.seekState != nil {
			r.seekState.percent = msg.progress
		}

		if msg.progress == 100 {
			return r, nil
		}

		return r, r.waitSeekProgress()
	case seekShowEvent:
		if !r.isSeeking {
			return r, nil
		}
		r.showSeek = true
		return r, nil
	case seekFinishEvent:
		r.handleSeek(msg.updateTime)

		if r.postSeekOptions == nil {
			return r, nil
		}

		for _, option := range r.postSeekOptions {
			option(r)
		}
		r.postSeekOptions = nil

		return r, nil
	case forceTimeEvent:
		return r, r.gotoIndex(msg.index, -1)
	case forceTimeDeltaEvent:
		return r, r.setTimeDelta(msg.delta, msg.skipInactivity)
	case ProgressEvent:
		r.progressPercent = msg.Percent
		return r, r.waitProgress()
	case PlaybackRateEvent:
		r.playbackRate = geom.Clamp(msg.Rate, -10, 10)
		if r.playbackRate == 0 {
			r.playbackRate = 1
		}
		return r, nil
	case frameDoneEvent:
		if !r.isPlaying {
			return r, nil
		}

		return r, r.timeStep(time.Since(msg.Start))
	case tea.WindowSizeMsg:
		// -3 for the " / " or " ? "
		r.input.Width = geom.Max(msg.Width-3, 0)
		r.resize(geom.Size{
			R: msg.Height,
			C: msg.Width,
		})
		if r.movement == nil {
			r.initializeMovement()
		}
		return r, nil
	case SearchResultEvent:
		return r, r.handleSearchResult(msg)
	}

	// TODO(cfoust): 11/10/23 this is actually wrong; if we do this,
	// ActionQuit does not go through
	if r.mode == ModeInput {
		return r.handleSearchInput(msg)
	}

	if r.mode == ModeCopy && r.incr.IsActive() {
		return r.handleIncrementalInput(msg)
	}

	// These events do not stop playback
	switch msg := msg.(type) {
	case ActionEvent:
		if r.isSeeking {
			break
		}

		if !r.historyLoaded && len(r.borgPath) > 0 {
			switch msg.Type {
			case ActionTimePlay:
				r.pendingMsg = msg
				r.loadErr = nil
				r.captureLoadRestore()
				return r, r.loadHistory()
			}
		}

		switch msg.Type {
		case ActionTimePlay:
			r.isPlaying = !r.isPlaying

			if !r.isPlaying {
				return r, nil
			}

			r.exitCopyMode()
			return r, r.timeStep(0)
		}
	case taro.KittyKeyMsg:
		// Clear out the "no matches" dialog
		r.isEmpty = false

		isTime := r.mode == ModeTime
		return r, func() tea.Msg {
			if isTime && r.timeBinds.InputMessage(msg) {
				return nil
			}
			r.copyBinds.InputMessage(msg)
			return nil
		}
	case bind.BindEvent:
		switch msg.Engine {
		case r.timeBinds:
			if r.mode == ModeCopy {
				return r, nil
			}

			return r, r.emit(msg)
		case r.copyBinds:
			r.mode = ModeCopy
			return r, r.emit(msg)
		}
		return r, nil
	}

	// Don't allow user input while we're seeking. While loading history from
	// disk, allow ActionQuit to cancel.
	if r.isSeeking {
		if r.loadingHistory {
			if action, ok := msg.(ActionEvent); ok &&
				action.Type == ActionQuit {
				r.pendingMsg = nil
				r.loadRestore = nil
				if r.seekState != nil {
					r.seekState.Cancel()
				}
			}
		}
		return r, nil
	}

	switch msg := msg.(type) {
	case CopyEvent:
		return r.handleCopy(msg.Register)
	case taro.MouseMsg:
		switch msg.Button {
		case keys.MouseWheelUp:
			before := r.getRoot()
			r.scrollYDelta(-1)
			after := r.getRoot()

			// If the scroll didn't move anywhere, we've hit the scrollback
			// limit and should load history from disk.
			if model, cmd := r.checkAndLoadHistory(before, after, msg); cmd != nil {
				return model, cmd
			}
		case keys.MouseWheelDown:
			r.scrollYDelta(+1)
		case keys.MouseWheelLeft:
			r.scrollXDelta(-1)
		case keys.MouseWheelRight:
			r.scrollXDelta(+1)
		case keys.MouseLeft:
			coord := r.movement.ViewportToMovement(msg.Vec2)

			switch msg.Type {
			case keys.MousePress:
				if !msg.Down {
					break
				}

				if !r.isCopyMode() {
					r.mode = ModeCopy
				}

				r.isSelecting = true
				r.movement.Goto(coord)
				r.selectStart = coord
			case keys.MouseMotion:
				if !r.isCopyMode() {
					break
				}

				if msg.Down && r.isSelecting {
					r.movement.Goto(coord)
				}
			}
		}
	case ActionEvent:
		if !r.historyLoaded && len(r.borgPath) > 0 {
			shouldLoad := false
			switch msg.Type {
			case ActionBeginning, ActionEnd:
				shouldLoad = !r.isCopyMode()
			case ActionSearchForward, ActionSearchBackward, ActionSearchAgain, ActionSearchReverse:
				shouldLoad = !r.isCopyMode()
			case ActionTimeStepBack, ActionTimeStepForward:
				shouldLoad = true
			case ActionTimePlay:
				shouldLoad = true
			case ActionCommandForward, ActionCommandBackward:
				shouldLoad = !r.isCopyMode()
			}

			if shouldLoad && msg.Type != ActionQuit {
				r.pendingMsg = msg
				r.loadErr = nil
				r.captureLoadRestore()
				return r, r.loadHistory()
			}
		}

		r.isPlaying = false
		switch msg.Type {
		case ActionQuit:
			// Ignore an in-progress search
			if r.isWaiting {
				r.isWaiting = false
				return r, nil
			}

			if r.isCopyMode() {
				if r.isSelecting {
					r.isSelecting = false
				} else {
					r.exitCopyMode()
				}
				return r, nil
			}

			return r.quit()
		case ActionBeginning:
			if r.isCopyMode() {
				before := r.getRoot()
				r.movement.ScrollTop()
				after := r.getRoot()

				if model, cmd := r.checkAndLoadHistory(before, after, msg); cmd != nil {
					return model, cmd
				}
				return r, nil
			}

			return r, r.gotoIndex(0, -1)
		case ActionEnd:
			if r.isCopyMode() {
				r.movement.ScrollBottom()
				return r, nil
			}
			return r, r.gotoIndex(-1, -1)
		case ActionSwapScreen:
			r.swapScreen()
			return r, nil
		case ActionSearchAgain, ActionSearchReverse:
			if r.isCopyMode() {
				r.incr.Next(
					r.movement,
					msg.Type == ActionSearchAgain,
				)
				return r, nil
			}

			return r, r.searchAgain(
				msg.Type != ActionSearchReverse,
			)
		case ActionSearchForward, ActionSearchBackward:
			if r.isCopyMode() {
				r.input.Reset()
				r.incr.Start(
					r.movement,
					msg.Type == ActionSearchForward,
				)
				return r, nil
			}

			if r.isWaiting {
				return r, nil
			}

			r.mode = ModeInput
			r.isForward = msg.Type == ActionSearchForward
			r.input.Reset()
			return r, nil
		case ActionTimeStepBack:
			return r, r.gotoIndex(r.Location().Index-1, -1)
		case ActionTimeStepForward:
			return r, r.gotoIndex(r.Location().Index+1, -1)
		case ActionScrollUpHalf:
			before := r.getRoot()
			r.moveCursorY(-(viewport.R / 2))
			after := r.getRoot()

			if model, cmd := r.checkAndLoadHistory(before, after, msg); cmd != nil {
				return model, cmd
			}
		case ActionScrollDownHalf:
			r.moveCursorY((viewport.R / 2))
		case ActionScrollUp:
			before := r.getRoot()
			r.scrollYDelta(-1)
			after := r.getRoot()

			if model, cmd := r.checkAndLoadHistory(before, after, msg); cmd != nil {
				return model, cmd
			}
		case ActionScrollDown:
			r.scrollYDelta(+1)
		case ActionScrollCursorTop:
			_, _, cursor := r.movement.Viewport()
			r.scrollYDelta(cursor.R)
		case ActionScrollCursorCenter:
			_, _, cursor := r.movement.Viewport()
			r.scrollYDelta(cursor.R - viewport.R/2)
		case ActionScrollCursorBottom:
			_, _, cursor := r.movement.Viewport()
			r.scrollYDelta(cursor.R - (viewport.R - 1))
		case ActionCursorDown:
			r.moveCursorY(1)
		case ActionCursorUp:
			before := r.getRoot()
			r.moveCursorY(-1)
			after := r.getRoot()

			if model, cmd := r.checkAndLoadHistory(before, after, msg); cmd != nil {
				return model, cmd
			}
		case ActionCursorLeft:
			r.moveCursorX(-1)
		case ActionCursorRight:
			r.moveCursorX(1)
		case ActionSelect:
			if !r.isCopyMode() {
				return r, nil
			}

			if r.isSelecting {
				r.isSelecting = false
				return r, nil
			}

			r.isSelecting = true
			r.selectStart = r.movement.Cursor()
		case ActionJumpReverse, ActionJumpAgain:
			if len(r.jumpChar) == 0 {
				return r, nil
			}

			direction := r.wasJumpForward
			if msg.Type == ActionJumpReverse {
				direction = !direction
			}

			r.handleJump(r.jumpChar, direction, r.wasJumpTo)
			return r, nil
		case ActionJumpForward, ActionJumpBackward, ActionJumpToForward, ActionJumpToBackward:
			isForward := msg.Type == ActionJumpForward || msg.Type == ActionJumpToForward
			isTo := msg.Type == ActionJumpToForward || msg.Type == ActionJumpToBackward
			r.handleJump(
				msg.Arg,
				isForward,
				isTo,
			)
			return r, nil
		case ActionWordForward, ActionWordBackward, ActionWordEndForward, ActionWordEndBackward:
			isForward := msg.Type == ActionWordForward || msg.Type == ActionWordEndForward
			isEnd := msg.Type == ActionWordEndForward || msg.Type == ActionWordEndBackward
			r.mode = ModeCopy
			motion.Word(
				r.movement,
				isForward,
				isEnd,
			)
			return r, nil
		case ActionBigWordForward, ActionBigWordBackward, ActionBigWordEndForward, ActionBigWordEndBackward:
			isForward := msg.Type == ActionBigWordForward || msg.Type == ActionBigWordEndForward
			isEnd := msg.Type == ActionBigWordEndForward || msg.Type == ActionBigWordEndBackward
			r.mode = ModeCopy
			motion.WORD(
				r.movement,
				isForward,
				isEnd,
			)
			return r, nil
		case ActionSelectInnerWord, ActionSelectInnerBigWord:
			isBig := msg.Type == ActionSelectInnerBigWord
			start, end, ok := motion.InnerWord(r.movement, isBig)
			if !ok {
				return r, nil
			}
			r.mode = ModeCopy
			r.isSelecting = true
			r.selectStart = start
			r.movement.Goto(end)
			return r, nil
		case ActionCommandForward, ActionCommandBackward:
			isForward := msg.Type == ActionCommandForward
			if !r.isCopyMode() {
				return r.jumpCommandTime(isForward)
			}

			return r.jumpCommand(isForward)
		case ActionCommandSelectForward, ActionCommandSelectBackward:
			isForward := msg.Type == ActionCommandSelectForward
			return r.jumpSelectCommand(isForward)
		}

		if motion, ok := MOTIONS[msg.Type]; ok {
			r.mode = ModeCopy
			motion(r.movement)
			return r, nil
		}
	}

	return r, nil
}
