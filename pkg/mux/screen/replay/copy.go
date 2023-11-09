package replay

import (
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/mattn/go-runewidth"
)

func normalizeRange(start, end geom.Vec2) (newStart, newEnd geom.Vec2) {
	if end.R < start.R || (end.R == start.R && end.C < start.C) {
		intermediate := start
		start = end
		end = intermediate
	}

	return start, end
}

// Read a starting from `start` to `end`, inclusive.
func (r *Replay) readString(start, end geom.Vec2) (result string) {
	start, end = normalizeRange(start, end)

	var char rune
	var startCol, endCol, lastChar int
	for row := start.R; row <= end.R; row++ {
		line := r.getLine(row)
		startCol = 0
		if row == start.R {
			startCol = start.C
		}

		_, lastChar = getNonWhitespace(line)
		endCol = lastChar
		if row == end.R {
			endCol = geom.Min(end.C, endCol)
		}

		for col := startCol; col <= endCol; col++ {
			char = line[col].Char
			result += string(char)

			w := runewidth.RuneWidth(char)
			for i := 1; i < w; i++ {
				col++
			}
		}

		if row != end.R && endCol == lastChar {
			result += "\n"
		}
	}

	return
}

func (r *Replay) handleCopy() (taro.Model, tea.Cmd) {
	if !r.isCopyMode() || !r.isSelecting {
		return r, nil
	}

	r.isSelecting = false
	text := r.readString(
		r.selectStart,
		r.viewportToTerm(r.cursor),
	)
	return r, func() tea.Msg {
		return taro.PublishMsg{
			Message: CopyEvent{
				Text: text,
			},
		}
	}
}
