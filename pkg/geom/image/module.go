package image

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
)

// An Image is a simple buffer of terminal state.
type Image [][]emu.Glyph

func (i Image) Size() geom.Vec2 {
	if len(i) == 0 {
		return geom.Vec2{}
	}

	return geom.Vec2{
		R: len(i),
		C: len(i[0]),
	}
}

func (i Image) Cell(x, y int) emu.Glyph {
	return i[y][x]
}

func New(size geom.Vec2) Image {
	image := Image{}

	for y := 0; y < size.R; y++ {
		line := make([]emu.Glyph, 0)
		for x := 0; x < size.C; x++ {
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
	cloned := New(size)
	Copy(cloned, 0, 0, i)
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

// Copy the cell contents from src to dst starting at [dstRow, dstCol],
func Copy(dst Image, dstRow, dstCol int, src Image) {
	srcSize := src.Size()
	dstSize := dst.Size()

	lastRow := geom.Min(
		dstRow+srcSize.R,
		dstSize.R,
	)
	lastCol := geom.Min(
		dstCol+srcSize.C,
		dstSize.C,
	)

	for row := dstRow; row < lastRow; row++ {
		for col := dstRow; col < lastCol; col++ {
			dst[row][col] = src[row-dstRow][col-dstCol]
		}
	}
}

// Like Copy, but does not overwrite a cell in dst if a cell in src is empty
// and has the default background.
func Compose(dst Image, dstRow, dstCol int, src Image) {
	srcSize := src.Size()
	dstSize := dst.Size()

	lastRow := geom.Min(
		dstRow+srcSize.R,
		dstSize.R,
	)
	lastCol := geom.Min(
		dstCol+srcSize.C,
		dstSize.C,
	)

	for row := dstRow; row < lastRow; row++ {
		for col := dstRow; col < lastCol; col++ {
			srcCell := src[row-dstRow][col-dstCol]
			if srcCell.Char == ' ' && srcCell.BG == emu.DefaultBG {
				continue
			}
			dst[row][col] = srcCell
		}
	}
}
