package search

import (
	"fmt"
	"regexp"
	"regexp/syntax"
	"sort"
	"strings"

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

func matchCell(re *regexp.Regexp, reader *ScreenReader, cell geom.Vec2) (loc []geom.Vec2, partial bool) {
	reader.Reset(cell)
	indices := re.FindReaderIndex(reader)

	// there was a full match
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

	loc = []geom.Vec2{
		cell,
		reader.Next(),
	}

	// If we read any more, there was a partial match no matter what
	partial = true
	return
}

func getFirst(re *syntax.Regexp) (string, error) {
	switch re.Op {
	case syntax.OpNoMatch, syntax.OpEmptyMatch:
		return "", fmt.Errorf("regex matches nothing")
	case syntax.OpAnyCharNotNL, syntax.OpAnyChar:
		// TODO(cfoust): 11/11/23 this should just be a warning
		return "", fmt.Errorf("regex starts with any char")
	case syntax.OpLiteral:
		return string(re.Rune[0]), nil
	case syntax.OpCharClass:
		return re.String(), nil
	case syntax.OpStar, syntax.OpPlus, syntax.OpQuest, syntax.OpRepeat:
		return re.Sub[0].String(), nil
	case syntax.OpConcat:
		return getFirst(re.Sub[0])
	case syntax.OpCapture:
		return getFirst(re.Sub[0])
	case syntax.OpAlternate:
		var subs []string
		for _, sub := range re.Sub {
			pattern, err := getFirst(sub)
			if err != nil {
				return "", err
			}
			subs = append(subs, pattern)
		}
		return fmt.Sprintf("(%s)", strings.Join(subs, "|")), nil
	default:
		return "", fmt.Errorf("initial operator not supported: %d", re.Op)
	}
}

func getPartial(pattern string) (*regexp.Regexp, error) {
	ast, err := syntax.Parse(pattern, syntax.Perl)
	if err != nil {
		return nil, err
	}

	first, err := getFirst(ast.Simplify())
	if err != nil {
		return nil, err
	}
	return regexp.Compile(first)
}

func Search(events []sessions.Event, pattern string, progress chan<- int) (results []SearchResult, err error) {
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

	first, err := getPartial(pattern[1:])
	if err != nil {
		return
	}

	checkpoints := make(map[int]map[int]struct{}, 0)
	for index, event := range events {
		output, ok := event.Message.(P.OutputMessage)
		if !ok {
			continue
		}

		for _, match := range first.FindAllIndex(output.Data, -1) {
			if _, ok := checkpoints[index]; !ok {
				checkpoints[index] = make(map[int]struct{}, 0)
			}

			checkpoints[index][match[0]] = struct{}{}
		}
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

			if !term.ScreenChanged() {
				continue
			}

			address = Address{
				Index:  index,
				Offset: offset,
			}

			cell, changed := term.LastCell()

			// Check all existing candidates
			for _, candidate := range candidates {
				if !candidate.Within(cell.Vec2, size) {
					continue
				}

				loc, partial := matchCell(re, reader, candidate.From)

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
				loc, partial := matchCell(re, reader, match.From)

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

			candidates = newCandidates
			matches = newMatches

			// Check for a new match
			if !changed {
				continue
			}

			indexEvents, ok := checkpoints[index]
			if !ok {
				continue
			}

			if _, ok := indexEvents[offset]; !ok {
				continue
			}

			loc, partial := matchCell(re, reader, cell.Vec2)

			if partial {
				candidate := Candidate{}
				candidate.From = loc[0]
				candidate.To = loc[1]
				newCandidates = append(
					newCandidates,
					candidate,
				)
			} else if len(loc) == 2 {
				result := SearchResult{}
				result.Begin = address
				result.From = loc[0]
				result.To = loc[1]
				newMatches = append(newMatches, result)
			}

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
