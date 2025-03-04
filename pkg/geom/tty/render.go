package tty

import (
	"bytes"
	"fmt"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"

	"github.com/xo/terminfo"
)

func setColor(info *terminfo.Terminfo, color emu.Color, isBg bool) []byte {
	data := new(bytes.Buffer)

	if (!isBg && color == emu.DefaultFG) || (isBg && color == emu.DefaultBG) {
		return make([]byte, 0)
	}

	// Special case for reversed text when still set to default
	if isBg && color == emu.DefaultFG {
		color = emu.ANSIColor(15)
	} else if !isBg && color == emu.DefaultBG {
		color = emu.ANSIColor(0)
	}

	if r, g, b, ok := color.RGB(); ok {
		if isBg {
			fmt.Fprintf(data, "\x1b[48;2;%d;%d;%dm", r, g, b)
		} else {
			fmt.Fprintf(data, "\x1b[38;2;%d;%d;%dm", r, g, b)
		}
	} else if xterm, ok := color.XTerm(); ok {
		code := terminfo.SetABackground
		if !isBg {
			code = terminfo.SetAForeground
		}

		info.Fprintf(data, code, xterm)
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

	for row := 0; row < max.R; row++ {
		for col := 0; col < max.C; col++ {
			dstCell := dst.Cell(col, row)
			srcCell := src.Cell(col, row)

			if dstCell.Equal(srcCell) {
				continue
			}

			info.Fprintf(data, terminfo.CursorAddress, row, col)

			mode := srcCell.Mode

			// note: emu.AttrReverse is handled virtually, since
			// it's just a color change

			if mode&emu.AttrBold != 0 {
				info.Fprintf(data, terminfo.EnterBoldMode)
			}

			if mode&emu.AttrUnderline != 0 {
				info.Fprintf(data, terminfo.EnterUnderlineMode)
			}

			if mode&emu.AttrStrikethrough != 0 {
				data.Write([]byte("\033[9m"))
			}

			if mode&emu.AttrItalic != 0 {
				// TODO(cfoust): 08/07/24 why does this not work?
				//info.Fprintf(data, terminfo.EnterItalicsMode)
				data.Write([]byte("\033[3m"))
			}

			if mode&emu.AttrBlink != 0 {
				info.Fprintf(data, terminfo.EnterBlinkMode)
			}

			data.Write(setColor(info, srcCell.FG, false))
			data.Write(setColor(info, srcCell.BG, true))

			data.Write([]byte(string(srcCell.Char)))

			// TODO(cfoust): 08/07/24 why does ExitAttributeMode not cover this in alacritty?
			if mode&emu.AttrStrikethrough != 0 {
				data.Write([]byte("\033[29m"))
			}

			info.Fprintf(data, terminfo.ExitAttributeMode)

			// CJK characters
			col += srcCell.Width() - 1
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

	dstCursor := dst.Cursor
	srcCursor := src.Cursor

	// TODO(cfoust): 08/09/23 debug
	//if dstCursor.X != srcCursor.X || dstCursor.Y != srcCursor.Y {
	info.Fprintf(data, terminfo.CursorAddress, srcCursor.R, srcCursor.C)

	if dstCursor.Style != srcCursor.Style {
		fmt.Fprintf(data, "\x1b[%d q", int(srcCursor.Style))
	}

	// This is wasteful, we shouldn't have to include this on every frame
	if src.CursorVisible {
		info.Fprintf(data, terminfo.CursorVisible)
	} else {
		info.Fprintf(data, terminfo.CursorInvisible)
	}

	return data.Bytes()
}
