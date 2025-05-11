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
		Background(s.params.SearchStatusBarBg()).
		Foreground(s.params.SearchStatusBarFg())
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

	p := s.params
	bar := renderBar(
		s.render,
		fmt.Sprintf(
			p.SearchTextSearching()+": '%s'",
			s.pendingQuery,
		),
		fmt.Sprintf("[%d/%d]", numComplete, numFiles),
		size.C,
	)

	emptyStyle := s.getBarStyle()
	filledStyle := s.render.NewStyle().
		Background(emptyStyle.GetForeground()).
		Foreground(emptyStyle.GetBackground())

	bar = taro.Progress(
		filledStyle, emptyStyle,
		bar,
		float64(numComplete)/float64(numFiles),
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
		Background(s.params.SearchStatusBarFg())
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
	defer s.params.ColorMap().Apply(state.Image)

	if s.searching {
		s.renderProgressBar(state)
		return
	}

	p := s.params
	if s.inputing {
		s.renderInput(state)
	} else if len(s.query) > 0 && len(s.complete) == 0 {
		s.renderStatusBar(
			state,
			fmt.Sprintf(
				p.SearchTextNoMatchesFound()+" '%s'",
				s.query,
			),
			"",
		)
		return
	}

	if s.loader == nil {
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
		s.loader.State(),
	)
}
