package search

import (
	"regexp"
	"sort"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/sessions"
)

type SearchResult struct {
	// The index of the event that
	Index int
	// The offset at which (after parsing) the match was on the screen
	Offset int
}

func matchCell(re *regexp.Regexp, reader *ScreenReader) (full, partial bool) {
	loc := re.FindReaderIndex(reader)

	if len(loc) != 0 {
		full = true
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
	re, err := regexp.Compile(pattern)
	if err != nil {
		return
	}

	term := emu.New()

	var flag emu.ChangeFlag
	var dirty []int

	reader := NewScreenReader(term, geom.Vec2{})

	// TODO(cfoust): 08/24/23 keep track of match candidates on non-dirty lines
	// right now we'll miss matches that start on non-dirty lines
	//candidates := make(map[geom.Vec2]struct{}, 32)

	for index, event := range events {
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

				cols, _ := term.Size()
				for _, row := range dirty {
					for col := 0; col < cols; col++ {
						reader.Reset(geom.Vec2{
							R: row,
							C: col,
						})
						full, _ := matchCell(
							re,
							reader,
						)

						if !full {
							continue
						}

						results = append(results, SearchResult{
							Index:  index,
							Offset: offset,
						})
					}
				}
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
