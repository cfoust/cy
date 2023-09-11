package taro

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"

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
func (r *Renderer) RenderAt(state *tty.State, row, col int, value string) {
	oldCols, oldRows := r.term.Size()
	newSize := state.Image.Size()

	if oldCols != newSize.C || oldRows != newSize.R {
		r.term.Resize(newSize.C, newSize.R)
	}

	r.info.Fprintf(r.term, terminfo.ClearScreen)
	r.info.Fprintf(r.term, terminfo.CursorHome)
	r.term.Write([]byte(value))

	image.Compose(geom.Vec2{R: row, C: col}, state.Image, r.term.Screen())
}

func NewRenderer() *Renderer {
	info, _ := terminfo.Load("xterm-256color")
	term := emu.New()

	return &Renderer{
		term:     term,
		info:     info,
		Renderer: lipgloss.NewRenderer(term, termenv.WithProfile(termenv.TrueColor)),
	}
}
