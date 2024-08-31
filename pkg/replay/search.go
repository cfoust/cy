package replay

import (
	"github.com/cfoust/cy/pkg/geom"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/sessions/search"
	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
)

func (r *Replay) gotoMatch(index int) tea.Cmd {
	if len(r.matches) == 0 {
		return nil
	}

	index = geom.Clamp(index, 0, len(r.matches)-1)
	match := r.matches[index].Begin
	return r.gotoIndex(match.Index, match.Offset)
}

func (r *Replay) searchAgain(isForward bool) tea.Cmd {
	events := r.Events()

	if r.isCopyMode() {
		return nil
	}

	matches := r.matches
	if len(matches) == 0 {
		return nil
	}

	if !r.isForward {
		isForward = !isForward
	}

	location := r.Location()

	firstMatch := matches[0].Begin
	lastMatch := matches[len(matches)-1].Begin

	if !isForward && (location.Before(firstMatch) || location.Equal(firstMatch)) {
		location.Index = len(events) - 1
		location.Offset = -1
	}

	// In order for the comparison to work, we have to turn our special -1
	// offset into a real value
	if location.Offset == -1 {
		event := events[location.Index]
		if output, ok := event.Message.(P.OutputMessage); ok {
			location.Offset = len(output.Data) - 1
		}
	}

	if isForward && (location.After(lastMatch) || location.Equal(lastMatch)) {
		location.Index = 0
		location.Offset = -1
	}

	var initialIndex int
	var other search.Address
	if isForward {
		for i, match := range matches {
			other = match.Begin
			if location.After(other) || location.Equal(other) {
				continue
			}
			initialIndex = i
			break
		}
	} else {
		for i := len(matches) - 1; i >= 0; i-- {
			other = matches[i].Begin
			if location.Before(other) || location.Equal(other) {
				continue
			}
			initialIndex = i
			break
		}
	}

	return r.gotoMatch(initialIndex)
}

type ProgressEvent struct {
	Percent int
}

func (r *Replay) waitProgress() tea.Cmd {
	if r.searchProgress == nil {
		return nil
	}

	return func() tea.Msg {
		return ProgressEvent{
			Percent: <-r.searchProgress,
		}
	}
}

func (r *Replay) handleSearchResult(msg SearchResultEvent) tea.Cmd {
	if r.isWaiting != true {
		return nil
	}

	r.isWaiting = false

	// TODO(cfoust): 10/13/23 handle error

	matches := msg.Results
	r.matches = matches
	if len(matches) == 0 {
		r.isEmpty = true
		return nil
	}

	r.isForward = msg.Forward
	return r.searchAgain(true)
}

func (r *Replay) handleSearchInput(msg tea.Msg) (taro.Model, tea.Cmd) {
	events := r.Events()
	switch msg := msg.(type) {
	case ActionEvent:
		switch msg.Type {
		case ActionQuit:
			r.mode = ModeTime
			return r, nil
		}
	case taro.KeyMsg:
		switch msg.Type {
		case taro.KeyEsc, taro.KeyCtrlC:
			r.mode = ModeTime
			return r, nil
		case taro.KeyEnter:
			value := r.searchInput.Value()
			r.searchInput.Reset()
			r.mode = ModeTime
			r.progressPercent = 0

			if match := TIME_DELTA_REGEX.FindStringSubmatch(value); match != nil {
				delta := parseTimeDelta(match)
				if !r.isForward {
					delta *= -1
				}
				return r, r.setTimeDelta(delta, false)
			}

			r.isWaiting = true
			r.matches = make([]search.SearchResult, 0)

			location := r.Location()
			isForward := r.isForward

			return r, tea.Batch(
				func() tea.Msg {
					res, err := search.Search(events, value, r.searchProgress)
					return SearchResultEvent{
						Forward: isForward,
						Origin:    location,
						Results:   res,
						err:       err,
					}
				},
				r.waitProgress(),
			)
		}
	}
	var cmd tea.Cmd
	inputMsg := msg
	if key, ok := msg.(taro.KeyMsg); ok {
		inputMsg = key.ToTea()
	}
	r.searchInput, cmd = r.searchInput.Update(inputMsg)
	return r, cmd
}
