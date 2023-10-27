package replay

import (
	"github.com/cfoust/cy/pkg/geom"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/sessions/search"
	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
)

func (r *Replay) gotoMatch(index int) {
	if len(r.matches) == 0 {
		return
	}

	index = geom.Clamp(index, 0, len(r.matches)-1)
	match := r.matches[index].Begin
	r.gotoIndex(match.Index, match.Offset)
}

func (r *Replay) searchAgain(isForward bool) {
	if r.isSelectionMode {
		return
	}

	matches := r.matches
	if len(matches) == 0 {
		return
	}

	if !r.isForward {
		isForward = !isForward
	}

	location := r.location

	firstMatch := matches[0].Begin
	lastMatch := matches[len(matches) - 1].Begin

	if !isForward && (location.Before(firstMatch) || location.Equal(firstMatch)) {
		location.Index = len(r.events) - 1
		location.Offset = -1
	}

	// In order for the comparison to work, we have to turn our special -1
	// offset into a real value
	if location.Offset == -1 {
		event := r.events[location.Index]
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

	r.gotoMatch(initialIndex)
}

func (r *Replay) handleSearchResult(msg SearchResultEvent) (taro.Model, tea.Cmd) {
	r.isWaiting = false

	// TODO(cfoust): 10/13/23 handle error

	matches := msg.results
	if len(matches) == 0 {
		r.matches = matches
		return r, nil
	}

	// TODO(cfoust): 10/27/23 y tho
	reverse(matches)

	r.matches = matches
	r.location = msg.origin
	r.isForward = msg.isForward
	r.searchAgain(true)
	return r, nil
}

func (r *Replay) handleSearchInput(msg tea.Msg) (taro.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case ActionEvent:
		switch msg.Type {
		case ActionQuit:
			r.isSearching = false
			return r, nil
		}
	case taro.KeyMsg:
		switch msg.Type {
		case taro.KeyEnter:
			value := r.searchInput.Value()

			r.searchInput.Reset()
			r.isWaiting = true
			r.isSearching = false
			r.matches = make([]search.SearchResult, 0)

			location := r.location
			isForward := r.isForward
			events := r.events

			return r, func() tea.Msg {
				res, err := search.Search(events, value)
				return SearchResultEvent{
					isForward: isForward,
					origin:    location,
					results:   res,
					err:       err,
				}
			}
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
