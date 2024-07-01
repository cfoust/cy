package replay

import (
	"time"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/geom"
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

type applyOptions struct {
	options []Option
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
	case applyOptions:
		for _, option := range msg.options {
			option(r)
		}
		return r, nil
	case seekEvent:
		r.handleSeek(msg.updateTime)
		return r, nil
	case ProgressEvent:
		r.progressPercent = msg.Percent
		return r, r.waitProgress()
	case PlaybackRateEvent:
		r.playbackRate = geom.Clamp(msg.Rate, -10, 10)
		if r.playbackRate == 0 {
			r.playbackRate = 1
		}
		return r, nil
	case PlaybackEvent:
		if !r.isPlaying {
			return r, nil
		}

		delta := int64(time.Now().Sub(msg.Since)) * int64(r.playbackRate)
		_, update := r.scheduleUpdate()
		return r, tea.Batch(
			r.setTimeDelta(
				time.Duration(delta),
				r.skipInactivity,
			),
			update,
		)
	case tea.WindowSizeMsg:
		// -3 for the " / " or " ? "
		r.incrInput.Width = geom.Max(msg.Width-3, 0)
		r.resize(geom.Size{
			R: msg.Height,
			C: msg.Width,
		})
		return r, nil
	case SearchResultEvent:
		return r, r.handleSearchResult(msg)
	}

	// Don't allow user input while we're seeking
	if r.isSeeking {
		return r, nil
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
		switch msg.Type {
		case ActionTimePlay:
			r.isPlaying = !r.isPlaying

			if r.isPlaying {
				r.exitCopyMode()
				return r.scheduleUpdate()
			}

			return r, nil
		}
	case taro.KeyMsg:
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

	switch msg := msg.(type) {
	case taro.MouseMsg:
		switch msg.Button {
		case taro.MouseWheelUp:
			r.scrollYDelta(-1)
		case taro.MouseWheelDown:
			r.scrollYDelta(+1)
		case taro.MouseWheelLeft:
			r.scrollXDelta(-1)
		case taro.MouseWheelRight:
			r.scrollXDelta(+1)
		}
	case ActionEvent:
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
				r.movement.ScrollTop()
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
				r.incrInput.Reset()
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
			r.searchInput.Reset()
		case ActionTimeStepBack:
			return r, r.gotoIndex(r.Location().Index-1, -1)
		case ActionTimeStepForward:
			return r, r.gotoIndex(r.Location().Index+1, -1)
		case ActionScrollUpHalf:
			r.moveCursorY(-(viewport.R / 2))
		case ActionScrollDownHalf:
			r.moveCursorY((viewport.R / 2))
		case ActionScrollUp:
			r.scrollYDelta(-1)
		case ActionScrollDown:
			r.scrollYDelta(+1)
		case ActionCursorDown:
			r.moveCursorY(1)
		case ActionCursorUp:
			r.moveCursorY(-1)
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
		case ActionCopy:
			return r.handleCopy()
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
