package image

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
)

// An Image is a simple buffer of terminal state.
type Image [][]emu.Glyph

func (i Image) Size() geom.Size {
	return geom.Size{
		Rows:    len(i),
		Columns: len(i[0]),
	}
}

func (i Image) Cell(x, y int) emu.Glyph {
	return i[y][x]
}

func New(columns, rows int) Image {
	image := Image{}

	for y := 0; y < rows; y++ {
		line := make([]emu.Glyph, 0)
		for x := 0; x < columns; x++ {
			line = append(line, emu.Glyph{
				Char: ' ',
			})
		}
		image = append(image, line)
	}

	return image
}

func (i Image) Clone() Image {
	size := i.Size()
	cloned := New(size.Columns, size.Rows)

	for y := 0; y < size.Rows; y++ {
		for x := 0; x < size.Columns; x++ {
			copied := i[y][x]
			cloned[y][x] = copied
		}
	}

	return cloned
}

func Capture(view emu.View) Image {
	image := Image{}

	view.Lock()
	defer view.Unlock()

	cols, rows := view.Size()
	for row := 0; row < rows; row++ {
		line := make([]emu.Glyph, cols)
		for col := 0; col < cols; col++ {
			line[col] = view.Cell(col, row)
		}
		image = append(image, line)
	}

	return image
}
