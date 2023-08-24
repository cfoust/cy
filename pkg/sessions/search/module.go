package search

import (
	"sort"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/sessions"
)

type SearchResult struct {
	// The index of the event that
	Index  int
	Offset int
}

func Search(events []sessions.Event, needle string) (results []SearchResult, err error) {
	term := emu.New()

	var flag emu.ChangeFlag
	var dirty []int

	for _, event := range events {
		switch event := event.Data.(type) {
		case sessions.OutputEvent:
			for offset := range event.Bytes {
				// Advance one byte at a time
				term.Write(event.Bytes[offset : offset+1])

				flag, dirty = term.Changes()
				if flag == 0 || len(dirty) == 0 {
					continue
				}

				sort.Ints(dirty)
			}
		case sessions.ResizeEvent:
			term.Resize(
				event.Columns,
				event.Rows,
			)
		}
	}
	return
}
