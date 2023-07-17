package tty

import (
	"bytes"
	"fmt"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"

	"github.com/xo/terminfo"
)

type State struct {
	Image         image.Image
	Cursor        emu.Cursor
	CursorVisible bool
}

func New(columns, rows int) *State {
	return &State{
		Image:         image.New(columns, rows),
		CursorVisible: true,
	}
}

func Capture(view emu.View) *State {
	view.Lock()
	cursor := view.Cursor()
	cursorVisible := view.CursorVisible()
	view.Unlock()

	return &State{
		Image:         image.Capture(view),
		Cursor:        cursor,
		CursorVisible: cursorVisible,
	}
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
func swapImage(
	info *terminfo.Terminfo,
	dst, src image.Image,
) []byte {
	data := new(bytes.Buffer)

	info.Fprintf(data, terminfo.CursorInvisible)

	max := geom.GetMaximum(dst.Size(), src.Size())

	for row := 0; row < max.Rows; row++ {
		for col := 0; col < max.Columns; col++ {
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

func Swap(
	info *terminfo.Terminfo,
	dst, src *State,
) []byte {
	data := new(bytes.Buffer)
	data.Write(swapImage(info, dst.Image, src.Image))

	// TODO(cfoust): 05/19/23 cursor mode?
	info.Fprintf(data, terminfo.CursorAddress, src.Cursor.Y, src.Cursor.X)

	return data.Bytes()
}
