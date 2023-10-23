package search

import (
	"fmt"
	"regexp"

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

// TODO(cfoust): 10/23/23 make this Before(), After(), Equal() to mimic Time API
func (a Address) Compare(other Address) int {
	if a == other {
		return 0
	}

	if a.Index < other.Index {
		return -1
	}

	if a.Index > other.Index {
		return 1
	}

	// index must be equal

	if a.Offset < other.Offset {
		return -1
	}

	if a.Offset > other.Offset {
		return 1
	}

	return 0
}

type SearchResult struct {
	// The location of the result in time
	Begin, End Address
	// The location of the result on the screen
	From, To geom.Vec2
}

func matchCell(re *regexp.Regexp, reader *ScreenReader, cell geom.Vec2) (loc []geom.Vec2, partial bool) {
	reader.Reset(cell)
	indices := re.FindReaderIndex(reader)

	if len(indices) == 2 {
		loc = []geom.Vec2{
			reader.GetLocation(indices[0]),
			reader.GetLocation(indices[1]),
		}
		return
	}

	// FindReaderIndex will always read three runes if you prefix with ^;
	// if that's all it read, there was no partial match
	if reader.NumRead() <= 3 {
		return
	}

	// If we read any more, there was a partial match no matter what
	partial = true
	return
}

func Search(events []sessions.Event, pattern string) (results []SearchResult, err error) {
	if len(pattern) == 0 {
		err = fmt.Errorf("pattern must be non-empty")
		return
	}

	// this MUST be set because of how the cell reader works
	if pattern[0] != '^' {
		pattern = "^" + pattern
	}

	re, err := regexp.Compile(pattern)
	if err != nil {
		return
	}

	term := emu.New()

	reader := NewScreenReader(term, geom.Vec2{}, geom.DEFAULT_SIZE.R*geom.DEFAULT_SIZE.C)

	// The partial matches we're tracking
	var candidates, newCandidates []geom.Vec2
	// The full matches we're tracking that are still on the screen
	var matches, newMatches []SearchResult

	var address Address

	for index, event := range events {
		if resize, ok := event.Message.(P.SizeMessage); ok {
			term.Resize(
				resize.Columns,
				resize.Rows,
			)
			reader.Resize(resize.Columns * resize.Rows)

			// TODO(cfoust): 08/24/23 clear all matches and
			// candidates, scan every cell for candidates
			continue
		}

		output := event.Message.(P.OutputMessage)

		for offset := range output.Data {
			newCandidates = make([]geom.Vec2, 0)
			newMatches = make([]SearchResult, 0)

			// Advance one byte at a time
			term.Write(output.Data[offset : offset+1])

			// TODO(cfoust): 08/25/23 if ChangeFlag is 0, don't bother

			address = Address{
				Index:  index,
				Offset: offset,
			}

			// Check all existing candidates
			for _, candidate := range candidates {
				loc, partial := matchCell(re, reader, candidate)

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

				// We have a full match!
				newMatches = append(
					newMatches,
					SearchResult{
						Begin: address,
						From:  loc[0],
						To:    loc[1],
					},
				)
			}

			for _, match := range matches {
				loc, partial := matchCell(re, reader, match.From)

				// Still track it if it was partial
				if partial {
					newCandidates = append(
						newCandidates,
						match.From,
					)
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

			// Check for a new match
			cell, changed := term.LastCell()
			if changed {
				loc, partial := matchCell(re, reader, cell.Vec2)

				if partial {
					newCandidates = append(
						newCandidates,
						cell.Vec2,
					)
				}

				if len(loc) == 2 {
					newMatches = append(
						newMatches,
						SearchResult{
							Begin: address,
							From:  loc[0],
							To:    loc[1],
						},
					)
				}
			}

			candidates = newCandidates
			matches = newMatches
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

	return
}
