package image

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
)

// An Image is a simple buffer of terminal state.
type Image []emu.Line

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
				FG:   emu.DefaultFG,
				BG:   emu.DefaultBG,
			})
		}
		image = append(image, line)
	}

	return image
}

func (i Image) Clone() Image {
	size := i.Size()
	cloned := New(size)
	Copy(geom.Vec2{}, cloned, i)
	return cloned
}

func Capture(view emu.View) Image {
	return view.Screen()
}

// Copy the cell contents from src to dst starting at [dstRow, dstCol],
func Copy(pos geom.Vec2, dst, src Image) {
	srcSize := src.Size()
	dstSize := dst.Size()

	lastRow := geom.Min(
		pos.R+srcSize.R,
		dstSize.R,
	)
	lastCol := geom.Min(
		pos.C+srcSize.C,
		dstSize.C,
	)

	for row := pos.R; row < lastRow; row++ {
		for col := pos.C; col < lastCol; col++ {
			dst[row][col] = src[row-pos.R][col-pos.C]
		}
	}
}

// Like Copy, but does not overwrite a cell in dst if a cell in src is empty
// and has the default background.
func Compose(pos geom.Vec2, dst, src Image) {
	srcSize := src.Size()
	dstSize := dst.Size()

	lastRow := geom.Min(
		pos.R+srcSize.R,
		dstSize.R,
	)
	lastCol := geom.Min(
		pos.C+srcSize.C,
		dstSize.C,
	)

	for row := pos.R; row < lastRow; row++ {
		for col := pos.C; col < lastCol; col++ {
			srcCell := src[row-pos.R][col-pos.C]
			if srcCell.Char == ' ' && srcCell.BG == emu.DefaultBG {
				continue
			}
			dst[row][col] = srcCell
		}
	}
}
