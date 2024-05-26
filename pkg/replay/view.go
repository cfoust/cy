package replay

import (
	"fmt"
	"time"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/replay/movement"

	"github.com/charmbracelet/bubbles/spinner"
	"github.com/charmbracelet/lipgloss"
)

func (r *Replay) getSearchHighlights() (highlights []movement.Highlight) {
	matches := r.matches
	if len(matches) == 0 {
		return
	}

	fgColor := r.render.ConvertLipgloss(lipgloss.Color("1"))
	bgColor := r.render.ConvertLipgloss(lipgloss.Color("14"))
	bgSelectedColor := r.render.ConvertLipgloss(lipgloss.Color("13"))

	location := r.Location()
	for _, match := range matches {
		// This match is not on the screen
		if location.Before(match.Begin) || location.After(match.End) {
			continue
		}

		bg := bgColor
		if location.Equal(match.Begin) {
			bg = bgSelectedColor
		}

		for _, appearance := range match.Appearances {
			if location.After(appearance.End) {
				continue
			}

			highlights = append(
				highlights,
				movement.Highlight{
					Screen: true,
					From:   appearance.From,
					To:     appearance.To,
					FG:     fgColor,
					BG:     bg,
				},
			)
			break
		}
	}
	return
}

func (r *Replay) drawStatusBar(state *tty.State) {
	size := state.Image.Size()

	statusBarStyle := r.render.NewStyle().
		Foreground(lipgloss.Color("15")).
		Background(lipgloss.Color("8"))

	statusText := "⏵"
	statusBG := lipgloss.Color("#4D9DE0")
	if r.isCopyMode() {
		statusText = "COPY"
		statusBG = lipgloss.Color("#E1BC29")

		if r.isSelecting {
			statusText = "VISUAL"
			statusBG = lipgloss.Color("#3BB273")
		}
	}
	if r.isPlaying {
		statusText = "⏸"
		statusBG = lipgloss.Color("#7768AE")
	}

	if !r.isCopyMode() && r.playbackRate != 1 {
		statusText += fmt.Sprintf(" %dx", r.playbackRate)
	}

	statusStyle := r.render.NewStyle().
		Inherit(statusBarStyle).
		Background(statusBG).
		Padding(0, 1)

	index := r.Location().Index
	events := r.Events()
	if index < 0 || index >= len(events) || len(events) == 0 {
		return
	}

	status := statusStyle.Render(statusText)

	leftSide := lipgloss.JoinHorizontal(lipgloss.Top,
		status,
		statusBarStyle.
			Copy().
			Padding(0, 1).
			Render(
				r.currentTime.Format(
					time.RFC3339,
				),
			),
	)

	progressWidth := size.C - lipgloss.Width(leftSide) - 3
	percent := int((float64(r.Location().Index) / float64(len(events))) * float64(progressWidth))
	progressBar := ""
	for i := 0; i < progressWidth; i++ {
		if i <= percent {
			progressBar += "▒"
		} else {
			progressBar += "-"
		}
	}

	progressBar = "[" + progressBar + "]"
	progressBar = statusBarStyle.
		Copy().
		Render(progressBar)

	statusBar := statusBarStyle.
		Width(size.C).
		Height(1).
		Render(lipgloss.JoinHorizontal(lipgloss.Left,
			leftSide,
			progressBar,
		))

	r.render.RenderAt(state.Image, size.R-1, 0, statusBar)
}

func (r *Replay) renderInput() image.Image {
	r.searchInput.Cursor.Style = r.render.NewStyle().
		Background(lipgloss.Color("15"))

	width := 20
	common := r.render.NewStyle().Width(width)
	inputStyle := common.Copy().
		Foreground(lipgloss.Color("15")).
		Background(lipgloss.Color("8"))

	promptStyle := common.Copy().
		Foreground(lipgloss.Color("8")).
		Background(lipgloss.Color("15"))

	prompt := "search-forward"
	if !r.isForward {
		prompt = "search-backward"
	}

	value := r.searchInput.Value()
	if match := TIME_DELTA_REGEX.FindStringSubmatch(value); len(value) > 0 && match != nil {
		promptStyle = common.Copy().
			Foreground(lipgloss.Color("15")).
			Background(lipgloss.Color("#7768AE"))

		prompt = "jump-forward"
		if !r.isForward {
			prompt = "jump-backward"
		}
	}

	input := inputStyle.Render(r.searchInput.View())

	if r.isWaiting {
		percent := r.progressPercent

		spin := spinner.Dot
		first := spin.Frames[percent%len(spin.Frames)]
		left := "searching..."
		prompt = left + lipgloss.PlaceHorizontal(
			width-len(left),
			lipgloss.Right,
			first,
		)

		progressStyle := inputStyle.Copy().
			Background(lipgloss.Color("#4D9DE0"))

		filled := int((float64(percent) / 100) * float64(width))

		input = progressStyle.Width(filled).Render("") + inputStyle.Width(width-filled).Render("")
	} else if r.isEmpty {
		prompt = "no matches found"
	}

	prompt = promptStyle.Render(prompt)

	input = lipgloss.JoinVertical(
		lipgloss.Left,
		input,
		prompt,
	)
	return r.render.RenderImage(input)
}

func (r *Replay) View(state *tty.State) {
	// Return nothing when View() is called before we've actually gotten
	// the viewport
	if r.viewport.R == 0 && r.viewport.C == 0 {
		return
	}

	highlights := r.getSearchHighlights()

	// Show the selection state
	////////////////////////////
	if r.isCopyMode() && r.isSelecting {
		highlights = append(
			highlights,
			movement.Highlight{
				From: r.selectStart,
				To:   r.movement.Cursor(),
				FG: r.render.ConvertLipgloss(
					lipgloss.Color("9"),
				),
				BG: r.render.ConvertLipgloss(
					lipgloss.Color("240"),
				),
			},
		)
	}

	if r.isFlowMode() && r.showCommands {
		for _, command := range r.Commands() {
			highlights = append(
				highlights,
				movement.Highlight{
					From: command.Output.From,
					To:   command.Output.To,
					FG: r.render.ConvertLipgloss(
						lipgloss.Color("9"),
					),
					BG: r.render.ConvertLipgloss(
						lipgloss.Color("113"),
					),
				},
			)

			for _, input := range command.Input {
				highlights = append(
					highlights,
					movement.Highlight{
						From: input.From,
						To:   input.To,
						FG: r.render.ConvertLipgloss(
							lipgloss.Color("9"),
						),
						BG: r.render.ConvertLipgloss(
							lipgloss.Color("160"),
						),
					},
				)
			}
		}
	}

	// Draw the terminal state
	///////////////////////////
	viewport := tty.New(r.viewport)
	r.movement.View(viewport, highlights)
	tty.Copy(geom.Vec2{}, state, viewport)
	state.CursorVisible = true

	if r.isPlaying {
		state.CursorVisible = r.CursorVisible()
	}

	// Render overlays
	///////////////////////////
	r.drawStatusBar(state)

	// Render text input
	/////////////////////////////
	if r.mode != ModeInput && !r.isWaiting && !r.isEmpty {
		return
	}

	// hide the cursor when typing in the search bar (it has its own)
	state.CursorVisible = false

	size := state.Image.Size()
	input := r.renderInput()
	inputSize := input.Size()
	image.Copy(
		geom.Vec2{
			// -1 for the status bar
			R: geom.Clamp(state.Cursor.R, 0, size.R-inputSize.R-1),
			C: geom.Clamp(state.Cursor.C, 0, size.C-inputSize.C),
		},
		state.Image,
		input,
	)
}
