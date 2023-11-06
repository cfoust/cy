package replay

import (
	"regexp"
	"strconv"
	"time"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
)

// Move the terminal back in time to the event at `index` and byte offset (if
// the event is an OutputMessage) of `indexByte`.
func (r *Replay) setIndex(index, indexByte int, updateTime bool) {
	numEvents := len(r.events)
	// Allow for negative indices from end of stream
	if index < 0 {
		index = geom.Clamp(numEvents+index, 0, numEvents-1)
	}

	fromIndex := geom.Clamp(r.location.Index, 0, numEvents-1)
	toIndex := geom.Clamp(index, 0, numEvents-1)
	fromByte := r.location.Offset
	toByte := indexByte

	// Going back in time; must start over
	if toIndex < fromIndex || (toIndex == fromIndex && toByte < fromByte) {
		r.terminal = emu.New()
		fromIndex = 0
		fromByte = -1
	}

	for i := fromIndex; i <= toIndex; i++ {
		event := r.events[i]
		switch e := event.Message.(type) {
		case P.OutputMessage:
			data := e.Data

			if toIndex == i {
				if toByte < 0 {
					toByte += len(data)
				}
				toByte = geom.Clamp(toByte, 0, len(data)-1)
			}

			if len(data) > 0 {
				if fromIndex == toIndex {
					data = data[fromByte+1 : toByte+1]
				} else if fromIndex == i {
					data = data[fromByte+1:]
				} else if toIndex == i {
					data = data[:toByte+1]
				}
			}

			r.terminal.Write(data)
		case P.SizeMessage:
			r.terminal.Resize(
				e.Columns,
				e.Rows,
			)
		}
	}

	r.location.Index = toIndex
	r.location.Offset = toByte
	if updateTime {
		r.currentTime = r.events[toIndex].Stamp
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
}

func (r *Replay) gotoIndex(index, indexByte int) {
	r.isPlaying = false
	r.setIndex(index, indexByte, true)
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
	TIME_DELTA_REGEX = regexp.MustCompile("^(?P<sign>[+-])((?P<days>\\d+)d)?((?P<hours>\\d+)h)?((?P<min>\\d+)m)?((?P<sec>\\d+)s)?$")
)

func parseTimeDelta(delta []string) (result time.Duration) {
	sign := delta[TIME_DELTA_REGEX.SubexpIndex("sign")]
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

	if sign == "-" {
		result *= -1
	}

	return
}

func (r *Replay) setTimeDelta(delta time.Duration, skipInactivity bool) {
	if len(r.events) == 0 {
		return
	}

	newTime := r.currentTime.Add(delta)
	if newTime.Equal(r.currentTime) {
		return
	}

	beginning := r.events[0].Stamp
	lastIndex := len(r.events) - 1
	end := r.events[lastIndex].Stamp
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
		indexStamp := r.events[currentIndex].Stamp
		for i := currentIndex; i >= 0; i-- {
			if newTime.Before(indexStamp) && newTime.After(r.events[i].Stamp) {
				nextIndex = i
				break
			}
		}
	} else {
		for i := r.location.Index + 1; i < len(r.events); i++ {
			if newTime.After(r.events[i].Stamp) {
				r.setIndex(i, -1, false)
				break
			}
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
		nextTime = r.events[currentIndex].Stamp
	} else {
		// we know `currentIndex` is not the last one because `end` is the time of the last event
		nextTime = r.events[currentIndex+1].Stamp
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
