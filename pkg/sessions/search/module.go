package search

import (
	"fmt"
	"regexp"
	"sort"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/sessions"
)

// Address refers to a point inside of a recording.
type Address struct {
	// The index of the event
	Index int
	// The byte offset at which (after parsing) the match either appeared
	// or disappeared
	Offset int
}

func (a Address) Before(other Address) bool {
	return a.Index < other.Index || (a.Index == other.Index && a.Offset < other.Offset)
}

func (a Address) After(other Address) bool {
	return a.Index > other.Index || (a.Index == other.Index && a.Offset > other.Offset)
}

func (a Address) Equal(other Address) bool {
	return a.Index == other.Index && a.Offset == other.Offset
}

type Selection struct {
	From, To geom.Vec2
}

// Within returns true if `pos` falls within the Selection.
func (s Selection) Within(pos geom.Vec2, size geom.Vec2) bool {
	var startCol, endCol int
	for row := s.From.R; row <= s.To.R; row++ {
		startCol = 0
		if row == s.From.R {
			startCol = s.From.C
		}

		endCol = size.C - 1
		if row == s.To.R {
			endCol = s.To.C
		}

		for col := startCol; col <= endCol; col++ {
			if pos.R == row && pos.C == col {
				return true
			}
		}
	}

	return false
}

type Candidate struct {
	Selection
}

type SearchResult struct {
	// The location of the result in time
	Begin, End Address
	// The location of the result on the screen
	Selection
}

func createLookup(matches []Match) map[Address]Match {
	lookup := make(map[Address]Match)
	for _, match := range matches {
		lookup[match.End] = match
	}
	return lookup
}

func Search(events []sessions.Event, pattern string, progress chan<- int) (results []SearchResult, err error) {
	if len(pattern) == 0 {
		err = fmt.Errorf("pattern must be non-empty")
		return
	}

	fullPattern, err := regexp.Compile(pattern)
	if err != nil {
		return
	}

	partialPattern, err := getPartial(pattern)
	if err != nil {
		return
	}

	s := NewSearcher()
	s.Parse(events)

	full := s.Find(fullPattern)
	fullLookup := createLookup(full)

	partial := s.Find(partialPattern)
	partialLookup := createLookup(partial)

	// Delete non-continuous matches from the full lookup, but add them to
	// partial
	for _, match := range full {
		if match.Continuous {
			continue
		}
		delete(fullLookup, match.End)
		partialLookup[match.Begin] = match
	}

	term := emu.New()
	term.EnableHistory(false)

	reader := NewScreenReader(term, geom.Vec2{}, geom.DEFAULT_SIZE)

	// The partial matches we're tracking
	var candidates, newCandidates []Candidate
	// The full matches we're tracking that are still on the screen
	var matches, newMatches []SearchResult

	var address Address

	size := geom.Vec2{}

	percent := 0
	for index, event := range events {
		newPercent := int(float64(index) / float64(len(events)) * 100)
		if newPercent > percent && progress != nil {
			percent = newPercent
			progress <- percent
		}

		if resize, ok := event.Message.(P.SizeMessage); ok {
			term.Resize(
				resize.Columns,
				resize.Rows,
			)
			size = geom.Vec2{C: resize.Columns, R: resize.Rows}
			reader.Resize(size)

			// TODO(cfoust): 08/24/23 clear all matches and
			// candidates, scan every cell for candidates
			continue
		}

		output := event.Message.(P.OutputMessage)

		for offset := range output.Data {
			newCandidates = make([]Candidate, 0)
			newMatches = make([]SearchResult, 0)

			// Advance one byte at a time
			term.Parse(output.Data[offset : offset+1])

			address = Address{
				Index:  index,
				Offset: offset,
			}

			if !term.ScreenChanged() {
				continue
			}

			cell, _ := term.LastCell()

			// Check all existing candidates
			for _, candidate := range candidates {
				if !candidate.Within(cell.Vec2, size) {
					continue
				}

				loc, partial := matchCell(fullPattern, reader, candidate.From)

				// The partial match is still partial
				if partial {
					newCandidates = append(
						newCandidates,
						candidate,
					)
					continue
				}

				// It left
				if len(loc) == 0 {
					continue
				}

				result := SearchResult{}
				result.Begin = address
				result.From = loc[0]
				result.To = loc[1]

				// We have a full match!
				newMatches = append(newMatches, result)
			}

			for _, match := range matches {
				if !match.Within(cell.Vec2, size) {
					newMatches = append(
						newMatches,
						match,
					)
					continue
				}
				loc, partial := matchCell(fullPattern, reader, match.From)

				// Still track it if it was partial
				if partial {
					candidate := Candidate{}
					candidate.From = loc[0]
					candidate.To = loc[1]
					newCandidates = append(newCandidates, candidate)
				}

				// It's gone
				if partial || len(loc) != 2 {
					match.End = address
					results = append(
						results,
						match,
					)
					continue
				}

				// Still a match, so leave it
				newMatches = append(
					newMatches,
					match,
				)
			}

			// to match end
			address.Offset += 1

			_, haveFull := fullLookup[address]
			_, havePartial := partialLookup[address]
			// Check for a new match
			if !haveFull && !havePartial {
				candidates = newCandidates
				matches = newMatches
				continue
			}

			if havePartial {
				candidate := Candidate{}
				//candidate.From = loc[0]
				//candidate.To = loc[1]
				newCandidates = append(
					newCandidates,
					candidate,
				)
			}

			if haveFull {
				result := SearchResult{}
				result.Begin = address
				//result.From = loc[0]
				//result.To = loc[1]
				newMatches = append(newMatches, result)
			}

			matches = newMatches
			candidates = newCandidates
		}
	}

	// Finish any matches that were done
	for _, match := range matches {
		match.End = address
		results = append(
			results,
			match,
		)
	}

	sort.SliceStable(results, func(i, j int) bool {
		return results[i].Begin.Before(results[j].Begin)
	})

	return
}
