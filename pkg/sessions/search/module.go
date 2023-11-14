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
	if pos.R < s.From.R || pos.R > s.To.R {
		return false
	}

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
	Cells []emu.Cell
}

type Appearance struct {
	Selection
	Begin, End Address
}

type SearchResult struct {
	// The location of the result in time
	Begin, End Address
	// The location of the result on the screen
	Appearances []Appearance
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

	s := NewSearcher()
	s.Parse(events)

	full := s.Find(fullPattern)
	if len(full) == 0 {
		return
	}

	term := emu.New()
	term.EnableHistory(false)

	reader := NewScreenReader(term, geom.Vec2{}, geom.DEFAULT_SIZE)

	// The full matches we're tracking that are still on the screen
	var matches, newMatches []SearchResult

	var address Address

	size := geom.Vec2{}

	nextFull := make([]SearchResult, 0)
	matchIndex := 0

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
			newMatches = make([]SearchResult, 0, len(matches))

			// Advance one byte at a time
			term.Parse(output.Data[offset : offset+1])

			address = Address{
				Index:  index,
				Offset: offset,
			}

			if !term.ScreenChanged() {
				continue
			}

			cell := term.LastCell()
			for _, match := range matches {
				appearances := match.Appearances
				if !appearances[len(appearances)-1].Within(cell.Vec2, size) {
					newMatches = append(
						newMatches,
						match,
					)
					continue
				}

				match.End = address
				match.Appearances[len(appearances)-1].End = address
				results = append(
					results,
					match,
				)
			}

			if len(nextFull) > 0 {
				realAddress := address
				realAddress.Offset++
				if realAddress == nextFull[0].Begin {
					fullEnd := nextFull[0]
					fullEnd.End = address
					fullEnd.Appearances[0].To = cell.Vec2
					nextFull = nextFull[1:]
					newMatches = append(newMatches, fullEnd)
				}
			}

			if matchIndex == len(full) || address != full[matchIndex].Begin {
				matches = newMatches
				continue
			}

			full := full[matchIndex]
			matchIndex++
			result := SearchResult{}
			result.Begin = full.End
			appearance := Appearance{}
			appearance.Begin = result.Begin
			appearance.From = cell.Vec2
			result.Appearances = append(result.Appearances, appearance)
			nextFull = append(nextFull, result)
			matches = newMatches
		}
	}

	// Finish any matches that were done
	for _, match := range matches {
		match.End = address
		match.Appearances[len(match.Appearances)-1].End = address
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
