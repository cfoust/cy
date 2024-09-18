package replay

import (
	"fmt"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/replay/detect"
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

// getCommand gets the command at the current location of the cursor, if any.
func (r *Replay) getCommand() (command detect.Command, ok bool) {
	cursor := r.movement.Cursor()
	for _, otherCommand := range r.Commands() {
		for _, input := range otherCommand.Input {
			if cursor.GTE(input.From) && cursor.LT(input.To) {
				return otherCommand, true
			}
		}

		if cursor.GTE(otherCommand.Output.From) && cursor.LT(otherCommand.Output.To) {
			return otherCommand, true
		}
	}
	return
}

func (r *Replay) getLeftStatusStyle() lipgloss.Style {
	p := r.params

	statusBG := p.ReplayTimeFg()
	if r.isCopyMode() {
		statusBG = p.ReplayCopyFg()

		if r.isSelecting {
			statusBG = p.ReplayVisualFg()
		}
	}
	if r.isPlaying {
		statusBG = p.ReplayPlayFg()
	}

	return r.render.NewStyle().
		Foreground(p.ReplayStatusBarFg()).
		Background(statusBG).
		Padding(0, 1)
}

func (r *Replay) drawStatusBar(state *tty.State) {
	p := r.params
	size := state.Image.Size()

	statusBarStyle := r.render.NewStyle().
		Foreground(r.params.ReplayStatusBarFg()).
		Background(r.params.ReplayStatusBarBg())

	statusText := "⏵"
	if r.isCopyMode() {
		statusText = "COPY"

		if r.isSelecting {
			statusText = "VISUAL"
		}
	}
	if r.isPlaying {
		statusText = "⏸"
	}

	if !r.isCopyMode() && r.playbackRate != 1 {
		statusText += fmt.Sprintf(" %dx", r.playbackRate)
	}

	leftStatusStyle := r.getLeftStatusStyle()

	if r.incr.IsActive() {
		r.incrInput.Cursor.Style = r.render.NewStyle().
			Background(p.ReplayStatusBarFg())
		r.incrInput.TextStyle = statusBarStyle
		r.incrInput.Cursor.TextStyle = statusBarStyle

		prefix := "/"
		if !r.incr.IsForward() {
			prefix = "?"
		}

		prefix = leftStatusStyle.Render(prefix)

		input := r.incrInput.View()

		statusBar := lipgloss.JoinHorizontal(lipgloss.Left,
			prefix,
			input,
		)

		r.render.RenderAt(
			state.Image,
			size.R-1, 0,
			statusBar,
		)
		return
	}

	leftStatus := leftStatusStyle.Render(statusText)
	if r.isFlowMode() && r.isCopyMode() {
		rightStyle := statusBarStyle.
			Width(size.C-lipgloss.Width(leftStatus)).
			Padding(0, 1)

		command, ok := r.getCommand()
		if ok {
			statusBar := lipgloss.JoinHorizontal(lipgloss.Left,
				leftStatus,
				rightStyle.Render(command.Text),
			)

			r.render.RenderAt(
				state.Image,
				size.R-1, 0,
				statusBar,
			)
			return
		}

		r.render.RenderAt(
			state.Image,
			size.R-1, 0,
			lipgloss.JoinHorizontal(lipgloss.Left,
				leftStatus,
				rightStyle.Render(""),
			),
		)
		return
	}

	index := r.Location().Index
	events := r.Events()
	if index < 0 || index >= len(events) || len(events) == 0 {
		return
	}

	timestamp := r.currentTime.Format(r.params.TimestampFormat())
	leftSide := lipgloss.JoinHorizontal(lipgloss.Top,
		leftStatus,
		statusBarStyle.
			Padding(0, 1).
			Render(timestamp),
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
	p := r.params
	r.searchInput.Cursor.Style = r.render.NewStyle().
		Background(p.ReplayStatusBarFg())

	statusFg := p.ReplayStatusBarFg()
	statusBg := p.ReplayStatusBarBg()

	width := 20
	common := r.render.NewStyle().Width(width)
	inputStyle := common.
		Foreground(statusFg).
		Background(statusBg)

	promptStyle := common.
		Foreground(statusBg).
		Background(statusFg)

	prompt := "search-forward"
	if !r.isForward {
		prompt = "search-backward"
	}

	value := r.searchInput.Value()
	if match := TIME_DELTA_REGEX.FindStringSubmatch(value); len(value) > 0 && match != nil {
		promptStyle = common.
			Foreground(statusFg).
			Background(p.ReplayTimeFg())

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

		progressStyle := inputStyle.
			Background(p.ReplayTimeFg())

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

func (r *Replay) renderSeek(state *tty.State) {
	seekState := r.seekState

	if seekState == nil {
		return
	}

	size := r.bg.Size()
	screen := seekState.screen
	if screen != nil {
		tty.Copy(
			geom.Vec2{},
			state,
			screen,
		)
	} else {
		image.Copy(
			geom.Vec2{},
			state.Image,
			r.bg,
		)
	}

	if !r.showSeek && screen != nil {
		return
	}

	stateSize := state.Image.Size()
	for row := 0; row < stateSize.R; row++ {
		for col := 0; col < stateSize.C; col++ {
			state.Image[row][col].FG = emu.ANSIColor(7)
		}
	}

	percent := int((float64(seekState.percent) / 100.) * float64(size.C))
	progressBar := ""
	for i := 0; i < size.C; i++ {
		if i <= percent {
			progressBar += "█"
		} else {
			progressBar += "▒"
		}
	}

	r.render.RenderAt(
		state.Image,
		state.Image.Size().R-1, 0,
		progressBar,
	)
}

func (r *Replay) View(state *tty.State) {
	// Return nothing when View() is called before we've actually gotten
	// the viewport
	if r.viewport.R == 0 && r.viewport.C == 0 {
		return
	}

	if r.isSeeking {
		r.renderSeek(state)
		return
	}

	highlights := r.getSearchHighlights()

	// Add in highlights provided with WithHighlights
	highlights = append(highlights, r.providedHighlights...)

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

	if highlight, ok := r.incr.Highlight(); ok {
		highlights = append(
			highlights,
			movement.Highlight{
				From: highlight.Root(),
				To: geom.Vec2{
					R: highlight.R,
					C: highlight.C1 - 1,
				},
				FG: r.render.ConvertLipgloss(
					lipgloss.Color("0"),
				),
				BG: r.render.ConvertLipgloss(
					lipgloss.Color("3"),
				),
			},
		)
	}

	// Only used for development (for now)
	if r.isFlowMode() && r.showCommands {
		commands := r.Commands()
		for _, command := range commands {
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

	if r.incr.IsActive() {
		state.CursorVisible = false
		return
	}

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
