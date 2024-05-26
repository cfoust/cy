package replay

import (
	"regexp"
	"strconv"
	"time"

	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
)

type seekEvent struct {
	updateTime bool
}

func (r *Replay) handleSeek(updateTime bool) {
	r.isSeeking = false
	r.isSwapped = false

	events := r.Events()
	location := r.Location()

	if updateTime {
		r.currentTime = events[location.Index].Stamp
	}

	r.mode = ModeTime
	r.initializeMovement()
}

// Move the terminal back in time to the event at `index` and byte offset (if
// the event is an OutputMessage) of `indexByte`.
func (r *Replay) setIndex(index, indexByte int, updateTime bool) tea.Cmd {
	r.isSeeking = true
	return func() tea.Msg {
		r.Goto(index, indexByte)
		return seekEvent{
			updateTime: updateTime,
		}
	}
}

func (r *Replay) gotoIndex(index, indexByte int) tea.Cmd {
	r.isPlaying = false
	return r.setIndex(index, indexByte, true)
}

// Jump to an index without using a tea.Cmd. Only used in testing.
func (r *Replay) forceIndex(index, indexByte int) {
	cmd := r.gotoIndex(index, indexByte)

	msg := cmd()
	if seek, ok := msg.(seekEvent); ok {
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

func (r *Replay) setTimeDelta(delta time.Duration, skipInactivity bool) tea.Cmd {
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

// forceTimeDelta synchronously adjusts time by `delta`. Only used in tests.
func (r *Replay) forceTimeDelta(delta time.Duration, skipInactivity bool) {
	cmd := r.setTimeDelta(delta, skipInactivity)

	msg := cmd()
	if seek, ok := msg.(seekEvent); ok {
		r.handleSeek(seek.updateTime)
	}
}
