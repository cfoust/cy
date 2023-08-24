package sessions

import (
	"sort"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"

	"github.com/mattn/go-runewidth"
)

type SearchResult struct {
	// The index of the event that
	Index  int
	Offset int
}

type searchCandidate struct {
	Text     string
	// Mapping from offset in string to location on screen
	Locs map[int]geom.Vec2
}

func buildCandidates(term emu.Terminal, dirty []int) (candidates []searchCandidate) {
	lines := make([]int, 0)
	var start, end int
	for i := 0; i < len(dirty); i++ {
		start = dirty[i]
		end = start + 1

		// Look ahead to group together adjacent lines
		for j := i+1; j < len(dirty); j++ {
			if dirty[j] != end {
				break
			}
			end = j
		}

		// Also include lines above and below
		if start > 0 {
		}
	}

	return
}

func Search(events []Event, needle string) (results []SearchResult) {
	term := emu.New()

	var flag emu.ChangeFlag
	var dirty []int

	for _, event := range events {
		switch event := event.Data.(type) {
		case OutputEvent:
			for offset := range event.Bytes {
				// Advance one byte at a time
				term.Write(event.Bytes[offset : offset+1])

				flag, dirty = term.Changes()
				if flag == 0 || len(dirty) == 0 {
					continue
				}

				sort.Ints(dirty)
			}
		case ResizeEvent:
			term.Resize(
				event.Columns,
				event.Rows,
			)
		}
	}
	return
}
