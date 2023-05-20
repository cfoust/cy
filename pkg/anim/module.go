package anim

import (
	"bytes"
	"unicode"

	"github.com/cfoust/cy/pkg/emu"

	"github.com/xo/terminfo"
)

type Image struct {
	grid          [][]emu.Glyph
	width, height int
}

func (i Image) Size() (int, int) {
	return i.width, i.height
}

func (i Image) Cell(x, y int) emu.Glyph {
	return i.grid[y][x]
}

func NewImage(width, height int) Image {
	image := Image{}

	for y := 0; y < height; y++ {
		line := make([]emu.Glyph, 0)
		for x := 0; x < width; x++ {
			line = append(line, emu.Glyph{
				Char: ' ',
			})
		}
		image.grid = append(image.grid, line)
	}

	image.width = width
	image.height = height

	return image
}

func (i Image) Clone() Image {
	width, height := i.Size()
	cloned := NewImage(width, height)

	for y := 0; y < height; y++ {
		for x := 0; x < width; x++ {
			cloned.grid[y][x] = i.grid[y][x]
		}
	}

	return cloned
}

func CaptureImage(view emu.View) Image {
	image := Image{}

	view.Lock()
	defer view.Unlock()

	width, height := view.Size()
	for y := 0; y < height; y++ {
		line := make([]emu.Glyph, width)
		for x := 0; x < width; x++ {
			line[x] = view.Cell(x, y)
		}
		image.grid = append(image.grid, line)
	}

	image.width = width
	image.height = height

	return image
}

// Calculate the minimum string to transform `src` in to `dst`.
func Swap(
	info *terminfo.Terminfo,
	dst, src Image,
) []byte {
	width, height := src.Size()
	data := new(bytes.Buffer)

	info.Fprintf(data, terminfo.CursorInvisible)

	for y := 0; y < height; y++ {
		for x := 0; x < width; x++ {
			dstCell := dst.Cell(x, y)
			srcCell := src.Cell(x, y)

			hasChar := dstCell.Char != srcCell.Char
			hasMode := dstCell.Mode != srcCell.Mode
			hasFG := dstCell.FG != srcCell.FG
			hasBG := dstCell.BG != srcCell.BG
			isDifferent := hasChar || hasMode || hasFG || hasBG

			if !isDifferent {
				continue
			}

			info.Fprintf(data, terminfo.CursorAddress, y, x)

			if hasMode {
				mode := dstCell.Mode
				if mode&emu.AttrReverse != 0 {
					info.Fprintf(data, terminfo.EnterReverseMode)
				}

				if mode&emu.AttrUnderline != 0 {
					info.Fprintf(data, terminfo.EnterUnderlineMode)
				}

				if mode&emu.AttrItalic != 0 {
					info.Fprintf(data, terminfo.EnterItalicsMode)
				}

				if mode&emu.AttrBlink != 0 {
					info.Fprintf(data, terminfo.EnterBlinkMode)
				}
			}

			if hasFG || hasBG {
				info.Fprintf(data, terminfo.SetAForeground, int(dstCell.FG))
				info.Fprintf(data, terminfo.SetABackground, int(dstCell.BG))
			}

			data.Write([]byte(string(dstCell.Char)))

			info.Fprintf(data, terminfo.ExitAttributeMode)
		}
	}

	info.Fprintf(data, terminfo.CursorNormal)

	return data.Bytes()
}

func SwapView(
	info *terminfo.Terminfo,
	dst, src emu.View,
) []byte {
	data := new(bytes.Buffer)
	data.Write(Swap(
		info,
		CaptureImage(dst),
		CaptureImage(src),
	))

	dstCursor := dst.Cursor()
	// TODO(cfoust): 05/19/23 cursor mode?
	info.Fprintf(data, terminfo.CursorAddress, dstCursor.Y, dstCursor.X)

	return data.Bytes()
}

type Animation interface {
	// delta is [0..1], indicates progress through animation
	Update(delta float32) Image
}

type CyFade struct {
	start Image
}

var _ Animation = (*CyFade)(nil)

func Fade(start Image) Animation {
	return &CyFade{
		start: start,
	}
}

func (c *CyFade) Update(delta float32) Image {
	output := c.start.Clone()

	end := 'a' + int32((delta/0.8)*25)
	mapping := make(map[rune]rune)
	for i := 'a'; i < end; i++ {
		target := 'c'
		if (i % 2) == 1 {
			target = 'y'
		}

		mapping[i] = target
		mapping[unicode.ToUpper(i)] = unicode.ToUpper(target)
	}

	width, height := output.Size()
	for y := 0; y < height; y++ {
		for x := 0; x < width; x++ {
			current := output.grid[y][x].Char

			if mapped, ok := mapping[current]; ok {
				output.grid[y][x].Char = mapped
			}
		}
	}

	return output
}
