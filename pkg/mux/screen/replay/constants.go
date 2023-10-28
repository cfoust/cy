package replay

import (
	"time"

	"github.com/cfoust/cy/pkg/sessions/search"
)

type SearchResultEvent struct {
	isForward bool
	origin    search.Address
	results   []search.SearchResult
	err       error
}

type ActionType int

type ActionEvent struct {
	Type ActionType
}

type PlaybackRateEvent struct {
	Rate int
}

const (
	PLAYBACK_FPS   = 30
	IDLE_THRESHOLD = time.Second
)

type PlaybackEvent struct {
	Since time.Time
}

type CopyEvent struct {
	Text string
}

type Mode uint8

const (
	// The default mode, Time, allows the user to navigate in time
	ModeTime Mode = iota
	// Copy mode occurs when the user moves the cursor or scrolls the
	// window
	ModeCopy
	// Input mode is used for searching
	ModeInput
)

const (
	ActionQuit ActionType = iota

	// Bimodal actions
	ActionBeginning
	ActionEnd
	ActionSearchForward
	ActionSearchBackward
	ActionSearchAgain
	ActionSearchReverse

	// Time controls have no tmux parallel
	//////////////////////////////////////
	ActionTimePlay
	ActionTimeStepBack
	ActionTimeStepForward

	// All tmux copy-mode commands, only some of which are implemented
	//////////////////////////////////////////////////////////////////
	// append-selection
	// append-selection-and-cancel                  A
	// back-to-indentation                          ^               M-m
	// begin-selection                              Space           C-Space
	// bottom-line                                  L
	// cancel                                       q               Escape
	// clear-selection                              Escape          C-g
	// copy-end-of-line [<prefix>]
	// copy-end-of-line-and-cancel [<prefix>]
	// copy-pipe-end-of-line [<command>] [<prefix>]
	// copy-pipe-end-of-line-and-cancel [<command>] [<prefix>] D               C-k
	// copy-line [<prefix>]
	// copy-line-and-cancel [<prefix>]
	// copy-pipe-line [<command>] [<prefix>]
	// copy-pipe-line-and-cancel [<command>] [<prefix>]
	// copy-pipe [<command>] [<prefix>]
	// copy-pipe-no-clear [<command>] [<prefix>]
	// copy-pipe-and-cancel [<command>] [<prefix>]
	// copy-selection [<prefix>]
	// copy-selection-no-clear [<prefix>]
	// copy-selection-and-cancel [<prefix>]         Enter           M-w
	ActionCopy
	// cursor-down                                  j               Down
	ActionCursorDown
	// cursor-down-and-cancel
	// cursor-left                                  h               Left
	ActionCursorLeft
	// cursor-right                                 l               Right
	ActionCursorRight
	// cursor-up                                    k               Up
	ActionCursorUp
	// end-of-line                                  $               C-e
	// goto-line <line>                             :               g
	// halfpage-down                                C-d             M-Down
	ActionScrollDownHalf
	// halfpage-down-and-cancel
	// halfpage-up                                  C-u             M-Up
	ActionScrollUpHalf
	// history-bottom                               G               M->
	// history-top                                  g               M-<
	// jump-again                                   ;               ;
	// jump-backward <to>                           F               F
	// jump-forward <to>                            f               f
	// jump-reverse                                 ,               ,
	// jump-to-backward <to>                        T
	// jump-to-forward <to>                         t
	// jump-to-mark                                 M-x             M-x
	// middle-line                                  M               M-r
	// next-matching-bracket                        %               M-C-f
	// next-paragraph                               }               M-}
	// next-space                                   W
	// next-space-end                               E
	// next-word                                    w
	// next-word-end                                e               M-f
	// other-end                                    o
	// page-down                                    C-f             PageDown
	// page-down-and-cancel
	// page-up                                      C-b             PageUp
	// pipe [<command>] [<prefix>]
	// pipe-no-clear [<command>] [<prefix>]
	// pipe-and-cancel [<command>] [<prefix>]
	// previous-matching-bracket                                    M-C-b
	// previous-paragraph                           {               M-{
	// previous-space                               B
	// previous-word                                b               M-b
	// rectangle-on
	// rectangle-off
	// rectangle-toggle                             v               R
	ActionSelect
	// refresh-from-pane                            r               r
	// scroll-down                                  C-e             C-Down
	ActionScrollDown
	// scroll-down-and-cancel
	// scroll-up                                    C-y             C-Up
	ActionScrollUp
	// search-again                                 n               n
	// search-backward <for>                        ?
	// search-backward-incremental <for>                            C-r
	// search-backward-text <for>
	// search-forward <for>                         /
	// search-forward-incremental <for>                             C-s
	// search-forward-text <for>
	// search-reverse                               N               N
	// select-line                                  V
	// select-word
	// set-mark                                     X               X
	// start-of-line                                0               C-a
	// stop-selection
	// toggle-position                              P               P
	// top-line                                     H               M-R
)
