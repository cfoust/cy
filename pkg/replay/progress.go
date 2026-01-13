package replay

import (
	"github.com/cfoust/cy/pkg/taro"

	"github.com/charmbracelet/lipgloss"
	"github.com/charmbracelet/x/ansi"
)

func renderProgressText(left, right string, width int) string {
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

// RenderProgressBar renders a single-line progress bar using taro.Progress. The
// returned string will have a width of `width` (or less if width is <= 0).
func RenderProgressBar(
	render *taro.Renderer,
	emptyStyle lipgloss.Style,
	width int,
	leftText, rightText string,
	percent float64,
) string {
	if width <= 0 || render == nil {
		return ""
	}

	filledStyle := render.NewStyle().
		Background(emptyStyle.GetForeground()).
		Foreground(emptyStyle.GetBackground())

	text := renderProgressText(leftText, rightText, width)
	return taro.Progress(
		filledStyle,
		emptyStyle,
		text,
		percent,
	)
}

