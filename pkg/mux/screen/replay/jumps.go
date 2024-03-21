package replay

import (
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/mattn/go-runewidth"
)

func (r *Replay) handleJump(needle string, isForward bool, isTo bool) (taro.Model, tea.Cmd) {
	oldPos := r.viewportToTerm(r.cursor)
	line := r.getImageLine(oldPos.R)

	newCol := oldPos.C
	if isForward {
		for i := oldPos.C + 1; i < len(line); i++ {
			char := line[i].Char

			if string(char) == needle {
				newCol = i
				break
			}

			w := runewidth.RuneWidth(char)
			for i := 1; i < w; i++ {
				i++
			}
		}
	} else {
		// Because of how rune width works, we can't iterate starting
		// from the current column
		lastCol := -1
		for i := 0; i < oldPos.C; i++ {
			char := line[i].Char

			if string(char) == needle {
				lastCol = i
				continue
			}

			w := runewidth.RuneWidth(char)
			for i := 1; i < w; i++ {
				i++
			}
		}

		if lastCol != -1 {
			newCol = lastCol
		}
	}

	// we set these (just like vim) and go into copy mode regardless
	r.jumpChar = needle
	r.wasJumpForward = isForward
	r.wasJumpTo = isTo
	r.mode = ModeCopy

	if newCol == oldPos.C {
		return r, nil
	}

	if isTo {
		if isForward {
			newCol--
		} else {
			newCol++
		}
	}

	newPos := geom.Vec2{
		R: oldPos.R,
		C: newCol,
	}
	r.moveCursorImage(newPos)
	r.desiredCol = r.termToViewport(newPos).C

	return r, nil
}
