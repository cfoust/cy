package search

import (
	"fmt"

	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/charmbracelet/lipgloss"
	"github.com/charmbracelet/x/ansi"
)

func (s *Search) getBarStyle() lipgloss.Style {
	return s.render.NewStyle().
		Background(lipgloss.Color("4")).
		Foreground(lipgloss.Color("15"))
}

// renderBar renders a status bar with a left and right side. The left side
// will be truncated with an ellipsis if it does not fit in the area left of
// the right side.
func renderBar(
	render *taro.Renderer,
	left, right string,
	width int,
) string {
	leftWidth := width - lipgloss.Width(right)
	left = lipgloss.PlaceHorizontal(
		leftWidth,
		lipgloss.Left,
		left,
	)
	left = ansi.Truncate(left, leftWidth, "…")
	left = render.NewStyle().
		MaxWidth(leftWidth).
		MaxHeight(1).
		Render(left)

	return lipgloss.JoinHorizontal(
		lipgloss.Left,
		left,
		right,
	)
}

func (s *Search) renderProgressBar(state *tty.State) {
	size := state.Image.Size()
	numFiles := len(s.pending)
	numComplete := 0
	for _, result := range s.pending {
		if result.Done {
			numComplete++
		}
	}

	bar := renderBar(
		s.render,
		fmt.Sprintf(
			"searching: '%s'",
			s.pendingQuery,
		),
		fmt.Sprintf("[%d/%d]", numComplete, numFiles),
		size.C,
	)

	emptyStyle := s.getBarStyle()
	filledStyle := s.render.NewStyle().
		Background(emptyStyle.GetForeground()).
		Foreground(emptyStyle.GetBackground())

	percent := float64(numComplete) / float64(numFiles)
	filledCells := int(percent * float64(size.C))

	var left, right string
	for i := len(bar); i >= 0; i-- {
		if lipgloss.Width(bar[:i]) > filledCells {
			continue
		}

		left = bar[:i]
		if i == len(bar) {
			right = ""
		} else {
			right = bar[i:]
		}

		left = filledStyle.Render(left)
		right = emptyStyle.Render(right)
		break
	}

	// Reconstitute the status bar
	bar = lipgloss.JoinHorizontal(
		lipgloss.Left,
		left,
		right,
	)

	s.render.RenderAt(
		state.Image,
		0, 0,
		bar,
	)
}

func (s *Search) renderInput(state *tty.State) {
	barStyle := s.getBarStyle()
	s.input.Cursor.Style = s.render.NewStyle().
		Background(lipgloss.Color("15"))
	s.input.TextStyle = barStyle
	s.input.Cursor.TextStyle = barStyle

	prefix := barStyle.Render("~>")
	input := s.input.View()

	statusBar := lipgloss.JoinHorizontal(lipgloss.Left,
		prefix,
		input,
	)

	s.render.RenderAt(
		state.Image,
		0, 0,
		statusBar,
	)
}

func (s *Search) renderStatusBar(
	state *tty.State,
	leftText, rightText string,
) {
	size := state.Image.Size()
	barStyle := s.getBarStyle()
	s.render.RenderAt(
		state.Image,
		0, 0,
		barStyle.Render(renderBar(
			s.render,
			leftText,
			rightText,
			size.C,
		)),
	)
}

func (s *Search) View(state *tty.State) {
	state.CursorVisible = false

	if s.searching {
		s.renderProgressBar(state)
		return
	}

	// empty matches
	if len(s.query) > 0 && len(s.complete) == 0 {
		s.renderStatusBar(
			state,
			fmt.Sprintf(
				"no matches found for '%s'",
				s.query,
			),
			"",
		)
		return
	}

	if s.inputing {
		s.renderInput(state)
	}

	if s.replay == nil {
		return
	}

	if !s.inputing {
		s.renderStatusBar(
			state,
			s.complete[s.selected].File,
			fmt.Sprintf(
				"[%d/%d]",
				s.selected+1,
				len(s.complete),
			),
		)
	}

	tty.Copy(
		s.inner.Position,
		state,
		s.replay.State(),
	)
}
