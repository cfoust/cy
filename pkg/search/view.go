package search

import (
	"fmt"

	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/charmbracelet/lipgloss"
)

func (s *Search) getBarStyle() lipgloss.Style {
	return s.render.NewStyle().
		Background(lipgloss.Color("4")).
		Foreground(lipgloss.Color("15"))
}

const (
	FILE_WAITING  = '_'
	FILE_COMPLETE = '▒'
	FILE_FOUND    = 'X'
)

func (s *Search) renderProgressBar(state *tty.State) {
	size := state.Image.Size()
	numFiles := len(s.pending)
	numComplete := 0
	for _, result := range s.pending {
		if result.Done {
			numComplete++
		}
	}

	right := fmt.Sprintf("[%d/%d]", numComplete, numFiles)

	leftWidth := size.C - lipgloss.Width(right)
	left := s.render.NewStyle().
		Width(leftWidth).
		MaxWidth(leftWidth).
		Render(fmt.Sprintf(
			"searching: '%s'",
			s.pendingQuery,
		))

	emptyStyle := s.getBarStyle()
	filledStyle := s.render.NewStyle().
		Background(emptyStyle.GetForeground()).
		Foreground(emptyStyle.GetBackground())

	// Calculate the bar the first time. This is done this way because we
	// want to account for characters wider than a single cell
	bar := lipgloss.JoinHorizontal(
		lipgloss.Left,
		left,
		right,
	)

	percent := float64(numComplete) / float64(numFiles)
	filledCells := int(percent * float64(size.C))

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

	leftWidth := size.C - lipgloss.Width(rightText)
	left := s.render.NewStyle().
		Width(leftWidth).
		MaxWidth(leftWidth).
		Render(leftText)

	s.render.RenderAt(
		state.Image,
		0, 0,
		barStyle.Render(lipgloss.JoinHorizontal(
			lipgloss.Left,
			left,
			rightText,
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
