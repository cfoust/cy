package overlay

import (
	"encoding/json"
	"math"
	"os"
	"sort"
	"strconv"
	"strings"
)

type cursorEvent struct {
	Time float64
	Row  int
	Col  int
	Drag bool
}

type cursorPos struct {
	Col, Row float64
	Drag     bool
	Visible  bool
}

// cursorTimeline is a sorted sequence of cursor events that supports
// interpolated lookup by time.
type cursorTimeline struct {
	events []cursorEvent
}

func newCursorTimeline(
	castFile string,
	speed float64,
) (*cursorTimeline, error) {
	events, err := parseCursorEvents(castFile)
	if err != nil {
		return nil, err
	}

	if speed > 0 && speed != 1.0 {
		for i := range events {
			events[i].Time /= speed
		}
	}

	return &cursorTimeline{events: events}, nil
}

func (t *cursorTimeline) Empty() bool {
	return len(t.events) == 0
}

// At returns the interpolated cursor position at time s.
func (t *cursorTimeline) At(s float64) cursorPos {
	idx := sort.Search(len(t.events), func(i int) bool {
		return t.events[i].Time > s
	}) - 1

	if idx < 0 {
		return cursorPos{}
	}

	entry := t.events[idx]
	col := float64(entry.Col)
	row := float64(entry.Row)

	if idx+1 < len(t.events) {
		next := t.events[idx+1]
		span := next.Time - entry.Time
		if span > 0 {
			frac := math.Min(1, math.Max(0,
				(s-entry.Time)/span,
			))
			col += (float64(next.Col) - col) * frac
			row += (float64(next.Row) - row) * frac
		}
	}

	return cursorPos{
		Col:     col,
		Row:     row,
		Drag:    entry.Drag,
		Visible: true,
	}
}

func parseCursorEvents(
	filename string,
) ([]cursorEvent, error) {
	data, err := os.ReadFile(filename)
	if err != nil {
		return nil, err
	}

	var events []cursorEvent
	for raw := range strings.SplitSeq(string(data), "\n") {
		line := strings.TrimSpace(raw)
		if line == "" {
			continue
		}

		var entry []any
		if err := json.Unmarshal(
			[]byte(line),
			&entry,
		); err != nil {
			continue
		}
		if len(entry) < 3 {
			continue
		}

		typeStr, ok := entry[1].(string)
		if !ok || typeStr != "c" {
			continue
		}

		stamp, ok := entry[0].(float64)
		if !ok {
			continue
		}
		payload, ok := entry[2].(string)
		if !ok {
			continue
		}

		parts := strings.Split(payload, ",")
		if len(parts) < 2 {
			continue
		}

		row, err := strconv.Atoi(parts[0])
		if err != nil {
			continue
		}
		col, err := strconv.Atoi(parts[1])
		if err != nil {
			continue
		}

		drag := len(parts) >= 3 && parts[2] == "d"

		events = append(events, cursorEvent{
			Time: stamp,
			Row:  row,
			Col:  col,
			Drag: drag,
		})
	}

	sort.Slice(events, func(i, j int) bool {
		return events[i].Time < events[j].Time
	})

	return events, nil
}
