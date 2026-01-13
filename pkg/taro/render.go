package taro

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"

	"github.com/charmbracelet/lipgloss"
	"github.com/charmbracelet/x/ansi"
	"github.com/muesli/termenv"
	"github.com/xo/terminfo"
)

// GetSize gets the size of `text` as it would appear on the screen.
func GetSize(text string) geom.Vec2 {
	return geom.Vec2{
		R: lipgloss.Height(text),
		C: lipgloss.Width(text),
	}
}

// Renderer makes it easy to render lipgloss strings.
type Renderer struct {
	*lipgloss.Renderer
	info *terminfo.Terminfo
	term emu.Terminal
}

// RenderAtSize renders the given string at (row, col) in `state` with a
// fixed terminal size.
func (r *Renderer) RenderAtSize(
	state image.Image,
	row, col int,
	rows, cols int,
	value string,
) {
	_, _ = r.term.Write([]byte("\033[2J"))        // clear screen
	_, _ = r.term.Write([]byte(emu.LineFeedMode)) // set CRLF mode
	size := r.term.Size()
	if cols > size.C || rows > size.R {
		r.term.Resize(geom.Size{C: cols, R: rows})
	}
	r.info.Fprintf(r.term, terminfo.ClearScreen)
	r.info.Fprintf(r.term, terminfo.CursorHome)
	_, _ = r.term.Write([]byte(value))
	image.Compose(geom.Vec2{R: row, C: col}, state, r.term.Screen())
}

// RenderAt renders the given string at (row, col) in `state`.
func (r *Renderer) RenderAt(state image.Image, row, col int, value string) {
	newCols := lipgloss.Width(value)
	newRows := lipgloss.Height(value)
	r.RenderAtSize(state, row, col, newRows, newCols, value)
}

func (r *Renderer) RenderImage(value string) image.Image {
	result := image.New(GetSize(value))
	r.RenderAt(result, 0, 0, value)
	return result
}

func (r *Renderer) LipglossToEmu(color lipgloss.Color) emu.Color {
	switch c := r.ColorProfile().Color(string(color)).(type) {
	case termenv.ANSIColor:
		return emu.ANSIColor(int(c))
	case termenv.ANSI256Color:
		return emu.XTermColor(int(c))
	case termenv.RGBColor:
		r, g, b := termenv.ConvertToRGB(c).RGB255()
		return emu.RGBColor(int(r), int(g), int(b))
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
		term:     emu.New(),
	}
}

// formatProgressText formats left and right text for a progress bar, ensuring
// they fit within the specified width. The right text is right-aligned and the
// left text is truncated if necessary.
func formatProgressText(left, right string, width int) string {
	if width <= 0 {
		return ""
	}

	if lipgloss.Width(right) > width {
		right = ansi.Truncate(right, width, "…")
	}

	rightWidth := lipgloss.Width(right)
	leftWidth := width - rightWidth
	if leftWidth < 0 {
		leftWidth = 0
	}

	left = ansi.Truncate(left, leftWidth, "…")
	left = lipgloss.PlaceHorizontal(leftWidth, lipgloss.Left, left)

	return lipgloss.JoinHorizontal(
		lipgloss.Left,
		left,
		right,
	)
}

// Progress applies a lipgloss.Style to a percentage of the input text.
func Progress(
	filledStyle, emptyStyle lipgloss.Style,
	text string,
	percent float64,
) string {
	if percent < 0 {
		percent = 0
	}
	if percent > 1 {
		percent = 1
	}

	filledCells := int(percent * float64(lipgloss.Width(text)))

	var left, right string
	for i := len(text); i >= 0; i-- {
		if lipgloss.Width(text[:i]) > filledCells {
			continue
		}

		left = text[:i]
		if i == len(text) {
			right = ""
		} else {
			right = text[i:]
		}

		left = filledStyle.Render(left)
		right = emptyStyle.Render(right)
		break
	}

	return lipgloss.JoinHorizontal(
		lipgloss.Left,
		left,
		right,
	)
}

// ProgressBar renders a single-line progress bar with left and right text.
// The returned string will have a width of `width` (or less if width is <= 0).
// The filled portion uses inverted colors from the empty style.
func (r *Renderer) ProgressBar(
	emptyStyle lipgloss.Style,
	width int,
	leftText, rightText string,
	percent float64,
) string {
	if width <= 0 {
		return ""
	}

	filledStyle := r.NewStyle().
		Background(emptyStyle.GetForeground()).
		Foreground(emptyStyle.GetBackground())

	text := formatProgressText(leftText, rightText, width)
	return Progress(
		filledStyle,
		emptyStyle,
		text,
		percent,
	)
}
