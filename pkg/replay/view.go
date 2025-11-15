package replay

import (
	"fmt"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/replay/detect"
	"github.com/cfoust/cy/pkg/replay/movement"
	"github.com/cfoust/cy/pkg/style"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/charmbracelet/lipgloss"
)

func (r *Replay) getSearchHighlights() (highlights []movement.Highlight) {
	matches := r.matches
	if len(matches) == 0 {
		return
	}

	p := r.params
	activeStyle := p.ReplayMatchActiveStyle()
	inactiveStyle := p.ReplayMatchInactiveStyle()

	var currentStyle *style.Style
	location := r.Location()
	for _, match := range matches {
		// This match is not on the screen
		if location.Before(match.Begin) || location.After(match.End) {
			continue
		}

		if location.Equal(match.Begin) {
			currentStyle = activeStyle
		} else {
			currentStyle = inactiveStyle
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
					Style:  currentStyle,
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

		if cursor.GTE(otherCommand.Output.From) &&
			cursor.LT(otherCommand.Output.To) {
			return otherCommand, true
		}
	}
	return
}

func (r *Replay) getLeftStatusStyle() lipgloss.Style {
	p := r.params

	var statusStyle = p.ReplayTimeStyle()
	if r.isCopyMode() {
		statusStyle = p.ReplayCopyStyle()

		if r.isSelecting {
			statusStyle = p.ReplayVisualStyle()
		}
	}
	if r.isPlaying {
		statusStyle = p.ReplayPlayStyle()
	}

	return statusStyle.Padding(0, 1)
}

func (r *Replay) drawStatusBar(state *tty.State) {
	p := r.params
	size := state.Image.Size()

	statusBarStyle := r.params.ReplayStatusBarStyle().Style

	statusText := p.ReplayTextTimeMode()
	if r.isCopyMode() {
		statusText = p.ReplayTextCopyMode()

		if r.isSelecting {
			statusText = p.ReplayTextVisualMode()
		}
	}
	if r.isPlaying {
		statusText = p.ReplayTextPlayMode()
	}

	if !r.isCopyMode() && r.playbackRate != 1 {
		statusText += fmt.Sprintf(" %dx", r.playbackRate)
	}

	leftStatusStyle := r.getLeftStatusStyle()

	if r.incr.IsActive() {
		r.renderIncremental(state, statusBarStyle)
		return
	}

	if r.mode == ModeInput || r.isWaiting || r.isEmpty {
		r.renderSearch(state, statusBarStyle)
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
	percent := int(
		(float64(r.Location().Index) / float64(len(events))) * float64(
			progressWidth,
		),
	)
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

func (r *Replay) renderIncremental(
	state *tty.State,
	statusBarStyle lipgloss.Style,
) {
	var (
		p               = r.params
		size            = state.Image.Size()
		leftStatusStyle = r.getLeftStatusStyle()
	)

	r.input.Cursor.Style = r.render.NewStyle().
		Background(p.ReplayStatusBarStyle().GetForeground())
	r.input.TextStyle = statusBarStyle
	r.input.Cursor.TextStyle = statusBarStyle

	prompt := "/"
	if !r.incr.IsForward() {
		prompt = "?"
	}

	prompt = leftStatusStyle.Render(prompt)

	input := r.input.View()

	statusBar := lipgloss.JoinHorizontal(lipgloss.Left,
		prompt,
		input,
	)

	r.render.RenderAt(
		state.Image,
		size.R-1, 0,
		statusBar,
	)
}

func (r *Replay) renderSearch(
	state *tty.State,
	statusBarStyle lipgloss.Style,
) {
	var (
		p               = r.params
		size            = state.Image.Size()
		leftStatusStyle = r.getLeftStatusStyle()
	)

	r.input.Cursor.Style = r.render.NewStyle().
		Background(p.ReplayStatusBarStyle().GetForeground())
	r.input.TextStyle = statusBarStyle
	r.input.Cursor.TextStyle = statusBarStyle

	if r.isWaiting {
		emptyStyle := p.ReplayTimeStyle().Style
		filledStyle := r.render.NewStyle().
			Background(emptyStyle.GetForeground()).
			Foreground(emptyStyle.GetBackground())

		percent := float64(r.progressPercent) / 100
		r.render.RenderAt(
			state.Image,
			size.R-1, 0,
			taro.Progress(
				filledStyle,
				emptyStyle,
				lipgloss.PlaceHorizontal(
					size.C,
					lipgloss.Left,
					"searching...",
				),
				percent,
			),
		)
		return
	}

	var prompt string
	if r.isEmpty {
		prompt = "no matches found"
	} else {
		prompt = "/"
		if !r.isForward {
			prompt = "?"
		}

		value := r.input.Value()
		if match := TIME_DELTA_REGEX.FindStringSubmatch(value); len(value) > 0 && match != nil {
			prompt = "jump-forward"
			if !r.isForward {
				prompt = "jump-backward"
			}
		}
	}

	prompt = leftStatusStyle.Render(prompt)

	input := r.input.View()

	statusBar := lipgloss.JoinHorizontal(lipgloss.Left,
		prompt,
		input,
	)

	r.render.RenderAt(
		state.Image,
		size.R-1, 0,
		statusBar,
	)
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

	p := r.params

	// Show the selection state
	////////////////////////////
	if r.isCopyMode() && r.isSelecting {
		highlights = append(
			highlights,
			movement.Highlight{
				From:  r.selectStart,
				To:    r.movement.Cursor(),
				Style: p.ReplaySelectionStyle(),
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
				Style: p.ReplayIncrementalStyle(),
			},
		)
	}

	// Only used for development (for now)
	if r.isFlowMode() && r.showCommands {
		commands := r.Commands()
		outputStyle := style.NewStyle(style.LightRed, style.NewColor("113"))
		inputStyle := style.NewStyle(style.LightRed, style.NewColor("160"))

		for _, command := range commands {
			highlights = append(
				highlights,
				movement.Highlight{
					From:  command.Output.From,
					To:    command.Output.To,
					Style: outputStyle,
				},
			)

			for _, input := range command.Input {
				highlights = append(
					highlights,
					movement.Highlight{
						From:  input.From,
						To:    input.To,
						Style: inputStyle,
					},
				)
			}
		}
	}

	// Draw the terminal state
	///////////////////////////
	viewport := tty.New(r.viewport)
	r.movement.View(r.params, viewport, highlights)
	tty.Copy(geom.Vec2{}, state, viewport)
	state.CursorVisible = true

	if r.isPlaying {
		state.CursorVisible = r.CursorVisible()
	}

	// Render overlays
	///////////////////////////
	r.drawStatusBar(state)

	if r.incr.IsActive() || r.mode == ModeInput {
		state.CursorVisible = false
	}
}
