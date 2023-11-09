package taro

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"

	"github.com/charmbracelet/lipgloss"
	"github.com/muesli/termenv"
	"github.com/xo/terminfo"
)

// Renderer makes it easy to render lipgloss strings.
type Renderer struct {
	*lipgloss.Renderer
	info *terminfo.Terminfo
	term emu.Terminal
}

// RenderAt renders the given string at (row, col) in `state`.
func (r *Renderer) RenderAt(state image.Image, row, col int, value string) {
	term := emu.New()
	term.Write([]byte("\033[20h")) // set CRLF mode
	newCols := lipgloss.Width(value)
	newRows := lipgloss.Height(value)
	term.Resize(newCols, newRows)
	r.info.Fprintf(term, terminfo.ClearScreen)
	r.info.Fprintf(term, terminfo.CursorHome)
	term.Write([]byte(value))

	image.Compose(geom.Vec2{R: row, C: col}, state, term.Screen())
}

func (r *Renderer) ConvertLipgloss(color lipgloss.Color) emu.Color {
	switch c := r.ColorProfile().Color(string(color)).(type) {
	case termenv.ANSIColor:
		return emu.Color(c)
	case termenv.ANSI256Color:
		return emu.Color(c)
	case termenv.RGBColor:
		r, g, b, _ := termenv.ConvertToRGB(c).RGBA()
		return emu.Color(r<<16 | g<<8 | b)
	}

	return emu.DefaultFG
}

func NewRenderer() *Renderer {
	info, _ := terminfo.Load("xterm-256color")
	renderer := lipgloss.NewRenderer(emu.New())
	renderer.SetColorProfile(termenv.TrueColor)

	return &Renderer{
		info:     info,
		Renderer: renderer,
	}
}
