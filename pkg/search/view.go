package search

import (
	"fmt"

	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/charmbracelet/lipgloss"
)

const (
	FILE_WAITING  = '_'
	FILE_COMPLETE = '▒'
	FILE_FOUND    = 'X'
)

func (s *Search) renderProgress(state *tty.State) {
	state.CursorVisible = false
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

	filledStyle := s.render.NewStyle().
		Background(lipgloss.Color("15")).
		Foreground(lipgloss.Color("4"))

	emptyStyle := s.render.NewStyle().
		Background(filledStyle.GetForeground()).
		Foreground(filledStyle.GetBackground())

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

func (s *Search) View(state *tty.State) {
	if s.searching {
		s.renderProgress(state)
		return
	}

	if s.replay == nil {
		return
	}

	tty.Copy(
		s.inner.Position,
		state,
		s.replay.State(),
	)
}
