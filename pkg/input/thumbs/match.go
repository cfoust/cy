package thumbs

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/sessions/search"
)

// getRegions gets all of the contiguous screen regions that should be checked for matches. This is useful only on image.Images produced by capturing the output of a LayoutEngine, since the Write field of each emu.Glyph will contain the ID of a pane.
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
