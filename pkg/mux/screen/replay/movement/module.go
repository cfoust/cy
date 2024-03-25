package movement

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/mattn/go-runewidth"
)

type Movement interface {
	Cursor() geom.Vec2
	Jump(needle string, isForward bool, isTo bool)
	MoveCursorX(delta int)
	MoveCursorY(delta int)
	ReadString(start, end geom.Vec2) string
	Reset()
	Resize(geom.Size)
	ScrollBottom()
	ScrollTop()
	ScrollXDelta(delta int)
	ScrollYDelta(delta int)
}

func getTerminalCursor(terminal emu.Terminal) geom.Vec2 {
	cursor := terminal.Cursor()
	return geom.Vec2{
		R: cursor.Y,
		C: cursor.X,
	}
}

func getTerminalSize(terminal emu.Terminal) geom.Vec2 {
	cols, rows := terminal.Size()
	return geom.Vec2{
		R: rows,
		C: cols,
	}
}

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
