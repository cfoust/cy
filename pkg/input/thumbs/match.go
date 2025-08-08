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

		pos.R++
		pos.C = startCol
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
