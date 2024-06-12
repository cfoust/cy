package motion

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"

	"github.com/mattn/go-runewidth"
)

func calculateJump(line emu.Line, needle string, isForward bool, isTo bool, oldPos int) int {
	newCol := oldPos
	if isForward {
		for i := oldPos + 1; i < len(line); i++ {
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
		for i := 0; i < oldPos; i++ {
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

	if isTo {
		if isForward {
			newCol--
		} else {
			newCol++
		}
	}

	return newCol
}

// Jump performs a jump that works identically to vim's fF/tT motions.
func Jump(s ScreenLike, needle string, isForward bool, isTo bool) {
	cursor := s.Cursor()
	line, ok := s.Line(cursor.R)
	if !ok {
		return
	}

	oldCol := cursor.C
	newCol := calculateJump(
		line,
		needle,
		isForward,
		isTo,
		oldCol,
	)
	s.Goto(geom.Vec2{
		C: newCol - oldCol,
		R: cursor.R,
	})
}
