package text

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"

	"github.com/charmbracelet/lipgloss"
)

func (t *Text) renderPrompt(prompt lipgloss.Style) string {
	leftSide := t.prompt

	return prompt.Render(
		lipgloss.JoinHorizontal(
			lipgloss.Left,
			leftSide,
		),
	)
}

func (t *Text) renderInputWindow(size geom.Size) image.Image {
	p := t.params
	commonStyle := t.render.NewStyle().Width(size.C)
	promptStyle := commonStyle.
		Background(p.InputPromptBg()).
		Foreground(p.InputPromptFg())

	promptStyle.GetBackground()
	arrow := t.render.NewStyle().
		Background(promptStyle.GetForeground()).
		Foreground(promptStyle.GetBackground()).
		Render("~>")

	inputStyle := t.render.NewStyle().
		Background(p.InputPromptFg()).
		Foreground(p.InputFindActiveBg())
	t.textInput.Cursor.Style = t.render.NewStyle().
		Background(p.InputFindActiveBg())
	t.textInput.TextStyle = inputStyle
	t.textInput.Cursor.TextStyle = inputStyle

	textInput := lipgloss.JoinHorizontal(lipgloss.Left,
		arrow,
		t.textInput.View(),
	)

	prompt := t.renderPrompt(promptStyle)

	output := lipgloss.JoinVertical(
		lipgloss.Left,
		textInput,
		prompt,
	)
	if t.isUp {
		output = lipgloss.JoinVertical(
			lipgloss.Left,
			prompt,
			textInput,
		)
	}

	window := image.New(geom.Size{
		R: geom.Min(lipgloss.Height(output), size.R),
		C: size.C,
	})
	t.render.RenderAt(window, 0, 0, output)

	return window
}

func (t *Text) View(state *tty.State) {
	if t.anim != nil {
		tty.Copy(geom.Vec2{}, state, t.anim.State())
	} else {
		size := state.Image.Size()
		for row := 0; row < size.R; row++ {
			for col := 0; col < size.C; col++ {
				state.Image[row][col].Mode |= emu.AttrTransparent
			}
		}
	}

	// the text input provides its own cursor
	state.CursorVisible = false

	screenSize := state.Image.Size()

	windowBounds := geom.Rect{
		Position: t.location,
		Size: geom.Vec2{
			R: screenSize.R - t.location.R,
			C: 50,
		},
	}

	if t.isUp {
		windowBounds.Position = geom.Vec2{
			R: 0,
			C: t.location.C,
		}
		windowBounds.Size.R = t.location.R
	}

	bottomRight := windowBounds.BottomRight()
	if bottomRight.C >= screenSize.C {
		windowBounds.Position.C -= bottomRight.C - screenSize.C
	}
	if bottomRight.R >= screenSize.R {
		windowBounds.Position.R -= bottomRight.R - screenSize.R
	}

	// Default to full screen if we can't fit the match window
	if !t.isInline || windowBounds.Position.C < 0 || windowBounds.Position.R < 0 {
		windowBounds.Size = screenSize
		windowBounds.Position = geom.Vec2{}
	}

	t.textInput.Width = windowBounds.Size.C - 2

	matchWindow := t.renderInputWindow(windowBounds.Size)

	emptyRows := windowBounds.Size.R - matchWindow.Size().R
	if t.isUp {
		windowBounds.Position.R += emptyRows
	}

	image.Copy(windowBounds.Position, state.Image, matchWindow)
}
