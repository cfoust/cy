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
	From geom.Vec2
	To   geom.Vec2
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

func (s Selection) WithinRect(rect geom.Rect, size geom.Vec2) bool {
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
			if rect.Contains(geom.Vec2{
				R: row,
				C: col,
			}) {
				return true
			}
		}
	}

	return false
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


// NormalizePattern returns the given pattern if it's a valid regex,
// otherwise it escapes all speical regex characters.
func NormalizePattern(pattern string) string {
	_, err := regexp.Compile(pattern)
	if err == nil {
		return pattern
	}

	return regexp.QuoteMeta(pattern)
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

	term := emu.New(emu.WithoutHistory)

	// The full matches we're tracking that are still on the screen
	var matches, newMatches []SearchResult

	var address Address

	size := geom.Vec2{}

	nextFull := make([]SearchResult, 0)
	matchIndex := 0

	dirty := term.Changes()

	percent := 0
	for index, event := range events {
		newPercent := int(float64(index) / float64(len(events)) * 100)
		if newPercent > percent && progress != nil {
			percent = newPercent
			progress <- percent
		}

		if resize, ok := event.Message.(P.SizeMessage); ok {
			term.Resize(resize.Vec())
			size = geom.Vec2{C: resize.Columns, R: resize.Rows}

			// TODO(cfoust): 08/24/23 clear all matches
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

			if !dirty.ScreenChanged() {
				continue
			}

			cell := dirty.Print
			scroll := dirty.Scroll
			if dirty.Scrolled {
				for _, match := range matches {
					appearance := match.Appearances[len(match.Appearances)-1]
					top := appearance.From.R
					bottom := appearance.To.R
					difference := bottom - top

					if scroll.Up {
						if top > scroll.Origin {
							top -= scroll.Count
						}
						if bottom > scroll.Origin {
							bottom -= scroll.Count
						}
					} else {
						if top > scroll.Origin {
							top += scroll.Count
						}
						if bottom > scroll.Origin {
							bottom += scroll.Count
						}
					}

					if top == appearance.From.R && bottom == appearance.To.R {
						newMatches = append(
							newMatches,
							match,
						)
						continue
					}

					if bottom-top != difference || top < 0 || bottom >= size.R {
						match.End = address
						match.Appearances[len(match.Appearances)-1].End = address
						results = append(
							results,
							match,
						)
						continue
					}

					match.Appearances[len(match.Appearances)-1].End = address
					newAppearance := Appearance{}
					newAppearance.Begin = address
					newAppearance.From = geom.Vec2{
						R: top,
						C: appearance.From.C,
					}
					newAppearance.To = geom.Vec2{
						R: bottom,
						C: appearance.To.C,
					}
					match.Appearances = append(match.Appearances, newAppearance)
					newMatches = append(
						newMatches,
						match,
					)
				}
			} else if dirty.Printed {
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
			} else if dirty.Cleared {
				for _, match := range matches {
					appearances := match.Appearances
					if !appearances[len(appearances)-1].WithinRect(dirty.Clear, size) {
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
			} else {
				newMatches = append(newMatches, matches...)			}

			if len(nextFull) > 0 {
				realAddress := address
				if realAddress == nextFull[0].Begin {
					fullEnd := nextFull[0]
					fullEnd.Appearances[0].To = cell.Vec2
					nextFull = nextFull[1:]
					newMatches = append(newMatches, fullEnd)
				}
			}

			dirty.Reset()

			if matchIndex == len(full) || address != full[matchIndex].Begin {
				matches = newMatches
				continue
			}

			full := full[matchIndex]
			matchIndex++
			result := SearchResult{}
			result.Begin = full.End
			result.Begin.Offset--
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
