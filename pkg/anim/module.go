package anim

import (
	"bytes"
	"fmt"
	"unicode"

	"github.com/cfoust/cy/pkg/emu"

	"github.com/xo/terminfo"
)

type Image struct {
	grid       [][]emu.Glyph
	cols, rows int
}

func (i Image) Size() (int, int) {
	return i.cols, i.rows
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

	image.cols = width
	image.rows = height

	return image
}

func (i Image) Clone() Image {
	width, height := i.Size()
	cloned := NewImage(width, height)

	for y := 0; y < height; y++ {
		for x := 0; x < width; x++ {
			copied := i.grid[y][x]
			cloned.grid[y][x] = copied
		}
	}

	return cloned
}

func CaptureImage(view emu.View) Image {
	image := Image{}

	view.Lock()
	defer view.Unlock()

	cols, rows := view.Size()
	for row := 0; row < rows; row++ {
		line := make([]emu.Glyph, cols)
		for col := 0; col < cols; col++ {
			line[col] = view.Cell(col, row)
		}
		image.grid = append(image.grid, line)
	}

	image.cols = cols
	image.rows = rows

	return image
}

func setColor(info *terminfo.Terminfo, color emu.Color, isBg bool) []byte {
	data := new(bytes.Buffer)
	num := uint32(color)
	maxColors := uint32(info.Nums[terminfo.MaxColors])

	if (!isBg && color == emu.DefaultFG) || (isBg && color == emu.DefaultBG) {
		return make([]byte, 0)
	}

	if num > maxColors {
		r := color >> 16
		g := (color >> 8) & 0xff
		b := color & 0xff

		if isBg {
			fmt.Fprintf(data, "\x1b[48;2;%d;%d;%dm", r, g, b)
		} else {
			fmt.Fprintf(data, "\x1b[38;2;%d;%d;%dm", r, g, b)
		}
	} else {
		code := terminfo.SetABackground
		if !isBg {
			code = terminfo.SetAForeground
		}

		info.Fprintf(data, code, int(color))
	}

	return data.Bytes()
}

// Calculate the minimum string to transform `src` in to `dst`.
func Swap(
	info *terminfo.Terminfo,
	dst, src Image,
) []byte {
	cols, rows := dst.Size()
	data := new(bytes.Buffer)

	info.Fprintf(data, terminfo.CursorInvisible)

	for row := 0; row < rows; row++ {
		for col := 0; col < cols; col++ {
			dstCell := dst.Cell(col, row)
			srcCell := src.Cell(col, row)

			if dstCell == srcCell {
				continue
			}

			info.Fprintf(data, terminfo.CursorAddress, row, col)

			mode := srcCell.Mode
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

			data.Write(setColor(info, srcCell.FG, false))
			data.Write(setColor(info, srcCell.BG, true))

			data.Write([]byte(string(srcCell.Char)))

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

	srcCursor := src.Cursor()
	// TODO(cfoust): 05/19/23 cursor mode?
	info.Fprintf(data, terminfo.CursorAddress, srcCursor.Y, srcCursor.X)

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

	end := 'a' + int32(delta*25)
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
			current := c.start.grid[y][x]

			if mapped, ok := mapping[current.Char]; ok {
				current.Char = mapped
				current.FG = c.start.grid[y][x].FG
			}

			output.grid[y][x] = current
		}
	}

	return output
}
