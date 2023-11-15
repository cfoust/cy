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

func (i Image) Clear(region geom.Rect) {
	size := i.Size()

	min := geom.Vec2{
		R: geom.Clamp(region.Position.R, 0, size.R-1),
		C: geom.Clamp(region.Position.C, 0, size.C-1),
	}
	max := geom.Vec2{
		R: geom.Clamp(region.Position.R+region.Size.R, 0, size.R),
		C: geom.Clamp(region.Position.C+region.Size.C, 0, size.C),
	}

	for row := min.R; row < max.R; row++ {
		for col := min.C; col < max.C; col++ {
			i[row][col] = emu.EmptyGlyph()
		}
	}
}

func Capture(view emu.View) Image {
	return view.Screen()
}

// Copy the cell contents from src to dst starting at [dstRow, dstCol],
func Copy(pos geom.Vec2, dst, src Image) {
	srcSize := src.Size()
	dstSize := dst.Size()
	if dstSize.IsZero() || srcSize.IsZero() {
		return
	}

	min := geom.Vec2{
		R: geom.Clamp(pos.R, 0, dstSize.R-1),
		C: geom.Clamp(pos.C, 0, dstSize.C-1),
	}
	max := geom.Vec2{
		R: geom.Clamp(pos.R+srcSize.R, 0, dstSize.R),
		C: geom.Clamp(pos.C+srcSize.C, 0, dstSize.C),
	}

	for row := min.R; row < max.R; row++ {
		for col := min.C; col < max.C; col++ {
			srcR := row - pos.R
			srcC := col - pos.C
			if srcR < 0 || srcC < 0 {
				continue
			}
			srcCell := src[row-pos.R][col-pos.C]
			if srcCell.Transparent {
				continue
			}
			dst[row][col] = srcCell
		}
	}
}

// Like Copy, but does not overwrite a cell in dst if a cell in src is empty
// and has the default background.
func Compose(pos geom.Vec2, dst, src Image) {
	srcSize := src.Size()
	dstSize := dst.Size()
	if dstSize.IsZero() || srcSize.IsZero() {
		return
	}

	min := geom.Vec2{
		R: geom.Clamp(pos.R, 0, dstSize.R-1),
		C: geom.Clamp(pos.C, 0, dstSize.C-1),
	}
	max := geom.Vec2{
		R: geom.Clamp(pos.R+srcSize.R, 0, dstSize.R),
		C: geom.Clamp(pos.C+srcSize.C, 0, dstSize.C),
	}

	for row := min.R; row < max.R; row++ {
		for col := min.C; col < max.C; col++ {
			srcR := row - pos.R
			srcC := col - pos.C
			if srcR < 0 || srcC < 0 {
				continue
			}
			srcCell := src[row-pos.R][col-pos.C]
			if srcCell.Char == ' ' && srcCell.BG == emu.DefaultBG {
				continue
			}
			dst[row][col] = srcCell
		}
	}
}
