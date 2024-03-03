package replay

import (
	"regexp"
	"strconv"
	"time"

	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
)

// Move the terminal back in time to the event at `index` and byte offset (if
// the event is an OutputMessage) of `indexByte`.
func (r *Replay) setIndex(index, indexByte int, updateTime bool) tea.Cmd {
	events := r.player.Events()

	// r.player.Goto(index, indexByte)

	location := r.player.Location()

	if updateTime {
		r.currentTime = events[location.Index].Stamp
	}

	r.recalculateViewport()
	termCursor := r.getTerminalCursor()
	viewportCursor := r.termToViewport(termCursor)

	r.mode = ModeTime

	// Center the cursor if it's not in the viewport
	if !r.isInViewport(viewportCursor) {
		r.centerPoint(termCursor)
	}

	r.cursor = r.termToViewport(termCursor)
	r.desiredCol = r.cursor.C

	return nil
}

func (r *Replay) gotoIndex(index, indexByte int) tea.Cmd {
	r.isPlaying = false
	return r.setIndex(index, indexByte, true)
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

func (r *Replay) setTimeDelta(delta time.Duration, skipInactivity bool) {
	events := r.player.Events()
	if len(events) == 0 {
		return
	}

	newTime := r.currentTime.Add(delta)
	if newTime.Equal(r.currentTime) {
		return
	}

	beginning := events[0].Stamp
	lastIndex := len(events) - 1
	end := events[lastIndex].Stamp
	if newTime.Before(beginning) || newTime.Equal(beginning) {
		r.gotoIndex(0, -1)
		return
	}

	if newTime.After(end) || newTime.Equal(end) {
		r.gotoIndex(lastIndex, -1)
		return
	}

	// First, just check to see whether we've entered another event
	currentIndex := r.location.Index
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
		for i := r.location.Index + 1; i < len(events); i++ {
			if newTime.Before(events[i].Stamp) {
				break
			}
			nextIndex = i
		}
	}

	// If this resulted in a change, we just jump to it immediately
	if currentIndex != nextIndex {
		r.currentTime = newTime
		r.setIndex(nextIndex, -1, false)
		return
	}

	if !skipInactivity {
		r.currentTime = newTime
		return
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
		return
	}

	if newTime.Before(r.currentTime) {
		r.setIndex(currentIndex-1, -1, false)
	} else {
		r.setIndex(currentIndex+1, -1, false)
	}
	r.currentTime = nextTime
}
