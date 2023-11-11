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
	"github.com/rs/zerolog/log"
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

type SearchResult struct {
	// The location of the result in time
	Begin, End Address
	// The location of the result on the screen
	From, To geom.Vec2
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
	log.Info().Msgf("%+v", first)
	if err != nil {
		return
	}

	initial := make([]Address, 0)
	for index, event := range events {
		output, ok := event.Message.(P.OutputMessage)
		if !ok {
			continue
		}

		for _, match := range first.FindAllIndex(output.Data, -1) {
			offset := match[0]
			initial = append(initial, Address{
				Index:  index,
				Offset: offset,
			})
		}
	}

	log.Info().Msgf("%d partial matches", len(initial))
	return

	term := emu.New()
	term.EnableHistory(false)

	reader := NewScreenReader(term, geom.Vec2{}, geom.DEFAULT_SIZE)

	// The partial matches we're tracking
	var candidates, newCandidates []geom.Vec2
	// The full matches we're tracking that are still on the screen
	var matches, newMatches []SearchResult

	var address Address

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
			reader.Resize(geom.Vec2{C: resize.Columns, R: resize.Rows})

			// TODO(cfoust): 08/24/23 clear all matches and
			// candidates, scan every cell for candidates
			continue
		}

		output := event.Message.(P.OutputMessage)

		for offset := range output.Data {
			newCandidates = make([]geom.Vec2, 0)
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

	sort.SliceStable(results, func(i, j int) bool {
		return results[i].Begin.Before(results[j].Begin)
	})

	return
}
