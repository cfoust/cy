package replay

import (
	"regexp"
	"strconv"
	"time"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	tea "github.com/charmbracelet/bubbletea"
)

// forceTimeEvent sets the time to the event at `index`. This is only used in
// testing.
type forceTimeEvent struct {
	index int
}

// forceTimeDeltaEvent moves time by `delta`. This is only used in testing.
type forceTimeDeltaEvent struct {
	delta          time.Duration
	skipInactivity bool
}

// setDelayEvent sets the amount of time to delay each progress update while
// seeking. This is only used in stories.
type setDelayEvent struct {
	delay time.Duration
}

// seekProgressEvent is sent when the progress of an ongoing seek operation
// changes.
type seekProgressEvent struct {
	progress int
}

// seekShowEvent is sent when the UI should update to reflect a seek; if a seek
// is short, we don't show anything.
type seekShowEvent struct{}

type seekFinishEvent struct {
	updateTime bool
}

type seekState struct {
	util.Lifetime
	percent  int
	progress chan int
	// While seeking, we cannot get the contents of the screen, so we use
	// the state of the screen before we started.
	screen *tty.State
}

func (r *Replay) handleSeek(updateTime bool) {
	r.isSeeking = false
	r.showSeek = false
	r.isSwapped = false

	r.seekState.Cancel()
	r.seekState = nil

	events := r.Events()
	location := r.Location()

	if updateTime {
		r.currentTime = events[location.Index].Stamp
	}

	r.mode = ModeTime
	r.initializeMovement()
}

// waitSeekProgress waits for the next progress event while seeking.
func (r *Replay) waitSeekProgress() tea.Cmd {
	seekState := r.seekState
	if seekState == nil {
		return nil
	}

	return func() tea.Msg {
		select {
		case <-seekState.Ctx().Done():
			return nil
		case p := <-seekState.progress:
			if r.seekDelay > 0 {
				time.Sleep(r.seekDelay)
			}

			return seekProgressEvent{
				progress: p,
			}
		}
	}
}

// Move the terminal back in time to the event at `index` and byte offset (if
// the event is an OutputMessage) of `indexByte`.
func (r *Replay) setIndex(index, indexByte int, updateTime bool) tea.Cmd {
	seekState := &seekState{
		Lifetime: util.NewLifetime(r.Ctx()),
		progress: make(chan int),
	}

	location := r.Location()
	viewport := tty.New(geom.Vec2{
		R: r.viewport.R + 1,
		C: r.viewport.C,
	})
	seekState.screen = viewport

	if r.movement != nil && location.Index != 0 && !r.viewport.IsZero() {
		r.View(viewport)
	} else {
		seekState.screen = nil
	}

	r.isSeeking = true
	r.showSeek = false
	r.seekState = seekState

	return tea.Batch(
		func() tea.Msg {
			r.GotoProgress(
				index,
				indexByte,
				seekState.progress,
			)

			return seekFinishEvent{
				updateTime: updateTime,
			}
		},
		r.waitSeekProgress(),
		func() tea.Msg {
			time.Sleep(SEEK_THRESHOLD)
			return seekShowEvent{}
		},
	)
}

func (r *Replay) gotoIndex(index, indexByte int) tea.Cmd {
	r.isPlaying = false
	return r.setIndex(index, indexByte, true)
}

// Jump to an index without using a tea.Cmd. Only used in testing.
func (r *Replay) forceIndex(index, indexByte int) {
	cmd := r.gotoIndex(index, indexByte)

	msg := cmd()
	if seek, ok := msg.(seekFinishEvent); ok {
		r.handleSeek(seek.updateTime)
	}
}

func (r *Replay) scheduleUpdate() (taro.Model, tea.Cmd) {
	since := time.Now()
	return r, func() tea.Msg {
		time.Sleep(time.Second / PLAYBACK_FPS)
		return PlaybackEvent{
			Since: since,
		}
	}
}

var (
	TIME_DELTA_REGEX = regexp.MustCompile("^((?P<days>\\d+)d)?((?P<hours>\\d+)h)?((?P<min>\\d+)m)?((?P<sec>\\d+)s)?$")
)

func parseTimeDelta(delta []string) (result time.Duration) {
	days := delta[TIME_DELTA_REGEX.SubexpIndex("days")]
	hours := delta[TIME_DELTA_REGEX.SubexpIndex("hours")]
	min := delta[TIME_DELTA_REGEX.SubexpIndex("min")]
	sec := delta[TIME_DELTA_REGEX.SubexpIndex("sec")]

	if len(days) > 0 {
		value, _ := strconv.Atoi(days)
		result += time.Duration(value) * 24 * time.Hour
	}

	if len(hours) > 0 {
		value, _ := strconv.Atoi(hours)
		result += time.Duration(value) * time.Hour
	}

	if len(min) > 0 {
		value, _ := strconv.Atoi(min)
		result += time.Duration(value) * time.Minute
	}

	if len(sec) > 0 {
		value, _ := strconv.Atoi(sec)
		result += time.Duration(value) * time.Second
	}

	return
}

func (r *Replay) setTimeDelta(
	delta time.Duration,
	skipInactivity bool,
) tea.Cmd {
	events := r.Events()
	if len(events) == 0 {
		return nil
	}

	newTime := r.currentTime.Add(delta)
	if newTime.Equal(r.currentTime) {
		return nil
	}

	beginning := events[0].Stamp
	lastIndex := len(events) - 1
	end := events[lastIndex].Stamp
	if newTime.Before(beginning) || newTime.Equal(beginning) {
		return r.gotoIndex(0, -1)
	}

	if newTime.After(end) || newTime.Equal(end) {
		return r.gotoIndex(lastIndex, -1)
	}

	// First, just check to see whether we've entered another event
	currentIndex := r.Location().Index
	var nextIndex int = currentIndex
	if newTime.Before(r.currentTime) {
		indexStamp := events[currentIndex].Stamp
		for i := currentIndex; i >= 0; i-- {
			if newTime.Before(indexStamp) && newTime.After(events[i].Stamp) {
				nextIndex = i
				break
			}
		}
	} else {
		for i := r.Location().Index + 1; i < len(events); i++ {
			if newTime.Before(events[i].Stamp) {
				break
			}
			nextIndex = i
		}
	}

	// If this resulted in a change, we just jump to it immediately
	if currentIndex != nextIndex {
		r.currentTime = newTime
		return r.setIndex(nextIndex, -1, false)
	}

	if !skipInactivity {
		r.currentTime = newTime
		return nil
	}

	// It didn't, which can only mean that we're waiting for the next event
	var nextTime time.Time
	if newTime.Before(r.currentTime) {
		nextTime = events[currentIndex].Stamp
	} else {
		// we know `currentIndex` is not the last one because `end` is the time of the last event
		nextTime = events[currentIndex+1].Stamp
	}

	if newTime.Sub(nextTime).Abs() < IDLE_THRESHOLD {
		r.currentTime = newTime
		return nil
	}

	currentTime := r.currentTime
	r.currentTime = nextTime

	if newTime.Before(currentTime) {
		return r.setIndex(currentIndex-1, -1, false)
	}

	return r.setIndex(currentIndex+1, -1, false)
}
