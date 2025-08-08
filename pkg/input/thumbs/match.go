package thumbs

import (
	"regexp"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/re"
	"github.com/cfoust/cy/pkg/sessions/search"
)

type thumbMatch []geom.Vec2

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
func convertMatch(region, raw search.Selection) (match thumbMatch) {
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
) (matches []thumbMatch) {
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

func findPatterns(
	patterns []*regexp.Regexp,
	i image.Image,
) (matches []thumbMatch) {
	var (
		regions    = getRegions(i)
		allMatches []thumbMatch
		occupied   = make(map[geom.Vec2]map[int]struct{})
	)

	// First get all the matches
	for _, pattern := range patterns {
		for _, region := range regions {
			for _, match := range findMatches(
				pattern,
				i,
				region,
			) {
				allMatches = append(
					allMatches,
					match,
				)
			}
		}
	}

	// Store a mapping from cell -> match indices
	for index, match := range allMatches {
		for _, cell := range match {
			occupancy, exists := occupied[cell]
			if !exists {
				occupancy = make(
					map[int]struct{},
				)
				occupied[cell] = occupancy
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

	// Just grab non-deleted matches
	for index, match := range allMatches {
		if _, ok := deleted[index]; ok {
			continue
		}

		matches = append(matches, match)
	}

	return
}
