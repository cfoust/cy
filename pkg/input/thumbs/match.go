package thumbs

import (
	"fmt"
	"regexp"
	"sort"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/re"
	"github.com/cfoust/cy/pkg/sessions/search"

	"github.com/mattn/go-runewidth"
)

type Match []geom.Vec2

// getRegions gets all of the contiguous screen regions that should be checked
// for matches. This is useful only on image.Images produced by capturing the
// output of a LayoutEngine, since the Write field of each emu.Glyph will
// contain the ID of a pane.
func getRegions(i image.Image) (regions []search.Selection) {
	size := i.Size()

	// The full image is always a region
	regions = append(regions, search.Selection{
		To: size,
	})

	// Look for subregions as delineated by WriteIDs
	panes := make(map[emu.WriteID]search.Selection)
	for row := range size.R {
		for col := range size.C {
			write := i[row][col].Write

			if write == 0 {
				continue
			}

			var (
				pos            = geom.Vec2{R: row, C: col}
				to             = pos.Add(geom.UnitVec2)
				region, exists = panes[write]
			)
			if !exists {
				panes[write] = search.Selection{
					From: pos,
					To:   to,
				}
				continue
			}

			region.To = to
			panes[write] = region
		}
	}

	for _, pane := range panes {
		regions = append(regions, pane)
	}

	return
}

// convertMatch turns the match inside of a region into a sequence of cells.
// This is because there might be arbitrarily many regions on the screen at
// once, in which we want to check for wrapped matches. convertMatch
// "normalizes" all discontiguous matches so that they're just a dumb sequence
// of cells.
func convertMatch(region, raw search.Selection) (match Match) {
	var (
		startCol = region.From.C
		endCol   = region.To.C
		pos      = raw.From
	)

	for {
		match = append(match, pos)

		pos.C++

		if pos == raw.To {
			break
		}

		if pos.C < endCol {
			continue
		}

		// Feed onto the next line
		pos.R++
		pos.C = startCol

		if pos == raw.To {
			break
		}
	}
	return
}

func findMatches(
	pattern *regexp.Regexp,
	i image.Image,
	region search.Selection,
) (matches []Match) {
	rawMatches := re.FindAllImage(
		pattern,
		i,
		region.From,
		region.To,
	)

	for _, match := range rawMatches {
		matches = append(
			matches,
			convertMatch(region, match),
		)
	}
	return
}

// Find searches `i` for all matches of `patterns` and resolves them (after
// deconfliction) into `Match`es.
func Find(
	patterns []*regexp.Regexp,
	i image.Image,
) (matches []Match) {
	var (
		regions    = getRegions(i)
		allMatches []Match
		occupied   = make(map[geom.Vec2]map[int]struct{})
	)

	// First get all the matches
	for _, pattern := range patterns {
		for _, region := range regions {
			allMatches = append(
				allMatches,
				findMatches(
					pattern,
					i,
					region,
				)...,
			)
		}
	}

	// Store a mapping from cell -> match indices
	for index, match := range allMatches {
		for _, cell := range match {
			if _, exists := occupied[cell]; !exists {
				occupied[cell] = make(
					map[int]struct{},
				)
			}

			occupied[cell][index] = struct{}{}
		}
	}

	// Deconflict matches by taking the longest that occupies a certain cell
	deleted := make(map[int]struct{})
	for _, conflicts := range occupied {
		if len(conflicts) == 1 {
			continue
		}

		var (
			longestIndex  = -1
			longestLength = -1
		)
		for matchIndex := range conflicts {
			if _, ok := deleted[matchIndex]; ok {
				continue
			}

			length := len(allMatches[matchIndex])
			if length < longestLength {
				continue
			}

			longestIndex = matchIndex
			longestLength = length
		}

		// All were deleted
		if longestIndex == -1 {
			continue
		}

		for matchIndex := range conflicts {
			if matchIndex == longestIndex {
				continue
			}
			deleted[matchIndex] = struct{}{}
		}
	}

	// Grab non-deleted matches
	for index, match := range allMatches {
		if _, ok := deleted[index]; ok {
			continue
		}

		// just type it bro
		if len(match) <= 4 {
			continue
		}

		matches = append(matches, match)
	}

	return
}

// generateHints creates hint strings for the given number of matches
func generateHints(alphabet []rune, numMatches int) []string {
	var (
		hints      = make([]string, 0, numMatches)
		numSymbols = len(alphabet)
		capacity   = numSymbols
		offset     = 0
	)

	// This is a quick way of calculating M s.t. |matches| <= N^M
	// Where N is the size of the alphabet
	// It enforces the constraint that no hint should be the prefix of
	// another hint
	for numMatches > capacity-offset {
		offset = capacity
		capacity *= numSymbols
	}

	for i := range numMatches {
		hint := ""
		num := i + offset
		for {
			hint = hint + string(alphabet[num%numSymbols])
			num = num / numSymbols
			if num == 0 {
				break
			}
			num--
		}
		hints = append(hints, hint)
	}

	return hints
}

// AssignHints assigns hints to `matches` by giving shorter hints to matches
// that are closer to `origin`.
func AssignHints(
	alphabet []rune,
	matches []Match,
	origin geom.Vec2,
) (hints map[string]Match) {
	hints = make(map[string]Match)

	distances := make([]int, len(matches))
	for index, match := range matches {
		if len(match) == 0 {
			distances[index] = 0
			continue
		}

		distances[index] = match[0].Dist(origin)
	}

	sort.SliceStable(matches, func(a, b int) bool {
		return distances[a] < distances[b]
	})

	hintStrings := generateHints(alphabet, len(matches))
	for index, match := range matches {
		hints[hintStrings[index]] = match
	}

	return
}

func ValidateAlphabet(alphabet []rune) error {
	if len(alphabet) < 2 {
		return fmt.Errorf(
			"alphabet must consist of two or more unique characters",
		)
	}

	chars := make(map[rune]struct{})

	for _, r := range alphabet {
		if _, ok := chars[r]; ok {
			return fmt.Errorf(
				"every symbol in alphabet must be unique",
			)
		}

		chars[r] = struct{}{}

		width := runewidth.RuneWidth(r)
		if width == 1 {
			continue
		}

		return fmt.Errorf(
			"invalid rune '%c': hint symbols must be one cell wide",
			r,
		)
	}

	return nil
}
