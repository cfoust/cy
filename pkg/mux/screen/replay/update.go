package replay

import (
	"time"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
)

func (r *Replay) quit() (taro.Model, tea.Cmd) {
	return r, tea.Quit
}

func (r *Replay) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	viewport := r.viewport

	switch msg := msg.(type) {
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
		r.setTimeDelta(
			time.Duration(delta),
			r.skipInactivity,
		)

		return r.scheduleUpdate()
	case tea.WindowSizeMsg:
		return r.setViewport(
			r.viewport,
			geom.Size{
				R: msg.Height,
				C: msg.Width,
			},
		)
	case SearchResultEvent:
		return r.handleSearchResult(msg)
	}

	if r.mode == ModeInput {
		return r.handleSearchInput(msg)
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
		// Pass unmatched keys into the binding engine; because of how
		// text input works, :replay bindings have to be activated
		// selectively
		return r, func() tea.Msg {
			r.binds.InputMessage(msg)
			return nil
		}
	}

	// Every other event causes us to pause
	r.isPlaying = false

	switch msg := msg.(type) {
	case taro.MouseMsg:
		switch msg.Type {
		case taro.MouseWheelUp:
			r.setScroll(r.offset.R - 1)
		case taro.MouseWheelDown:
			r.setScroll(r.offset.R + 1)
		}
	case ActionEvent:
		switch msg.Type {
		case ActionQuit:
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
				r.moveCursorDelta(
					-r.viewportToTerm(r.cursor).R+r.minOffset.R,
					0,
				)
			} else {
				r.gotoIndex(0, -1)
			}
		case ActionEnd:
			if r.isCopyMode() {
				r.moveCursorDelta(
					(r.getTerminalSize().R-1)-r.viewportToTerm(r.cursor).R,
					0,
				)
			} else {
				r.gotoIndex(-1, -1)
			}
		case ActionSearchAgain, ActionSearchReverse:
			r.searchAgain(msg.Type != ActionSearchReverse)
		case ActionSearchForward, ActionSearchBackward:
			r.mode = ModeInput
			r.isForward = msg.Type == ActionSearchForward
			r.searchInput.Reset()
		case ActionTimeStepBack:
			r.gotoIndex(r.location.Index-1, -1)
		case ActionTimeStepForward:
			r.gotoIndex(r.location.Index+1, -1)
		case ActionScrollUpHalf:
			r.moveCursorDelta(-(viewport.R / 2), 0)
		case ActionScrollDownHalf:
			r.moveCursorDelta((viewport.R / 2), 0)
		case ActionScrollUp:
			r.setScroll(r.offset.R - 1)
		case ActionScrollDown:
			r.setScroll(r.offset.R + 1)
		case ActionCursorDown:
			r.moveCursorDelta(1, 0)
		case ActionCursorUp:
			r.moveCursorDelta(-1, 0)
		case ActionCursorLeft:
			r.moveCursorDelta(0, -1)
		case ActionCursorRight:
			r.moveCursorDelta(0, 1)
		case ActionSelect:
			if !r.isCopyMode() {
				return r, nil
			}

			if r.isSelecting {
				r.isSelecting = false
				return r, nil
			}

			r.isSelecting = true
			r.selectStart = r.viewportToTerm(r.cursor)
		case ActionCopy:
			return r.handleCopy()
		}
	}

	return r, nil
}
