package fuzzy

import (
	"fmt"
	"strings"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"

	"github.com/charmbracelet/lipgloss"
	"github.com/charmbracelet/lipgloss/table"
)

// Return an image representing the contents of the preview window.
func (f *Fuzzy) getPreviewContents() (preview image.Image) {
	options := f.getOptions()
	if len(options) == 0 {
		return
	}

	option := options[f.selected]
	if option.Preview == nil {
		return
	}

	if f.preview == nil {
		return
	}

	return f.preview.State().Image
}

func (f *Fuzzy) renderPreview(state *tty.State) {
	size := state.Image.Size()
	contents := f.getPreviewContents()
	if contents == nil {
		return
	}

	previewSize := contents.Size()
	previewPos := size.Center(previewSize)
	state.Image.Clear(geom.Rect{
		Position: previewPos,
		Size:     previewSize,
	})
	image.Copy(previewPos, state.Image, contents)

	p := f.params
	border := f.render.NewStyle().
		Border(p.InputPreviewBorder().Border).
		BorderForeground(p.InputPreviewBorderFg()).
		BorderTop(true).
		BorderLeft(true).
		BorderRight(true).
		BorderBottom(true).
		Width(previewSize.C).
		Height(previewSize.R).
		Render("")

	f.render.RenderAt(
		state.Image,
		previewPos.R-1, previewPos.C-1,
		border,
	)
}

func (f *Fuzzy) renderOptions(
	common, prompt lipgloss.Style,
	options []Option,
	maxOptions int,
) string {
	// the prompt otherwise comes with a fixed size, which messes with the
	// table
	prompt = prompt.Width(0)

	p := f.params
	rowStyle := f.render.NewStyle()
	inactive := rowStyle.
		Background(p.InputFindInactiveBg()).
		Foreground(p.InputFindInactiveFg())
	active := rowStyle.
		Background(p.InputFindActiveBg()).
		Foreground(p.InputFindActiveFg())

	var rows [][]string

	headers := f.headers
	haveHeaders := len(f.headers) != 0
	// The table library requires headers, even if we skip them
	if len(headers) == 0 {
		headers = []string{""}
	}

	if haveHeaders {
		maxOptions -= 1
	}

	windowOffset := geom.Clamp(
		f.selected-(maxOptions/2),
		0,
		geom.Max(0, len(options)-maxOptions),
	)
	windowEnd := geom.Min(len(options), windowOffset+maxOptions)

	numColumns := 0
	for index, option := range options[windowOffset:windowEnd] {
		columns := append(
			[]string{},
			option.Columns...,
		)

		if len(columns) == 0 {
			columns = []string{option.Text}
		}

		// Add in the > in front of each row
		prefix := "  "
		if windowOffset+index == f.selected {
			prefix = "> "
		}
		columns[0] = prefix + columns[0]

		numColumns = geom.Max(numColumns, len(columns))
		rows = append(rows, columns)
	}

	table := table.New().
		Border(lipgloss.Border{}).
		StyleFunc(func(row, col int) lipgloss.Style {
			switch row {
			case table.HeaderRow:
				return prompt
			case (f.selected - windowOffset):
				return active
			default:
				return inactive
			}
		}).
		Headers(headers...).
		Width(common.GetWidth() + 2 + geom.Max(numColumns-1, 0)).
		Rows(rows...)

	rendered := strings.Split(table.String(), "\n")

	if len(rendered) < 4 {
		return ""
	}

	// lipgloss adds some blank lines that we want to skip
	var output []string
	if haveHeaders {
		output = append(
			output,
			rendered[1],
		)
	}

	// Invert the lines of the table when `isUp`
	lines := make([]string, 0)
	for _, line := range rendered[3 : len(rendered)-1] {
		if f.isUp {
			lines = append([]string{line}, lines...)
		} else {
			lines = append(lines, line)
		}
	}
	output = append(output, lines...)

	return strings.Join(output, "\n")
}

func (f *Fuzzy) renderPrompt(prompt lipgloss.Style) string {
	numFiltered := len(f.filtered)
	if numFiltered == 0 && len(f.textInput.Value()) == 0 {
		numFiltered = len(f.options)
	}

	leftSide := f.prompt
	rightSide := fmt.Sprintf(
		"%d/%d",
		numFiltered,
		len(f.options),
	)

	return prompt.Render(
		lipgloss.JoinHorizontal(
			lipgloss.Left,
			leftSide,
			lipgloss.PlaceHorizontal(
				prompt.GetWidth()-lipgloss.Width(leftSide),
				lipgloss.Right,
				rightSide,
			),
		),
	)
}

func (f *Fuzzy) renderMatchWindow(size geom.Size) image.Image {
	p := f.params
	commonStyle := f.render.NewStyle().Width(size.C)
	promptStyle := commonStyle.
		Background(p.InputPromptBg()).
		Foreground(p.InputPromptFg())

	// TODO(cfoust): 07/20/24 handle this more gracefully
	if size.R < 2 {
		return image.New(geom.Size{})
	}

	var lines []string
	if options := f.getOptions(); len(options) > 0 {
		optionsLines := f.renderOptions(
			commonStyle,
			promptStyle,
			options,
			size.R-2,
		)
		f.numRenderedOptions = lipgloss.Height(optionsLines)
		lines = append(lines, optionsLines)
	}

	promptStyle.GetBackground()
	arrow := f.render.NewStyle().
		Background(promptStyle.GetForeground()).
		Foreground(promptStyle.GetBackground()).
		Render("~>")

	inputStyle := f.render.NewStyle().
		Background(p.InputPromptFg()).
		Foreground(p.InputFindActiveBg())
	f.textInput.Cursor.Style = f.render.NewStyle().
		Background(p.InputFindActiveBg())
	f.textInput.TextStyle = inputStyle
	f.textInput.Cursor.TextStyle = inputStyle

	textInput := lipgloss.JoinHorizontal(lipgloss.Left,
		arrow,
		f.textInput.View(),
	)

	prompt := f.renderPrompt(promptStyle)

	if f.isUp {
		lines = append(lines, prompt, textInput)
	} else {
		lines = append([]string{
			textInput,
			prompt,
		}, lines...)
	}

	output := lipgloss.JoinVertical(lipgloss.Left, lines...)

	window := image.New(geom.Size{
		R: geom.Min(lipgloss.Height(output), size.R),
		C: size.C,
	})
	f.render.RenderAt(window, 0, 0, output)

	return window
}

func (f *Fuzzy) View(state *tty.State) {
	if f.anim != nil {
		tty.Copy(geom.Vec2{}, state, f.anim.State())
	} else {
		size := state.Image.Size()
		for row := 0; row < size.R; row++ {
			for col := 0; col < size.C; col++ {
				state.Image[row][col].Mode |= emu.AttrTransparent
			}
		}
	}

	if f.haveMoved {
		f.renderPreview(state)
	}

	// the text input provides its own cursor
	state.CursorVisible = false

	screenSize := state.Image.Size()

	windowBounds := geom.Rect{
		Position: f.location,
		Size: geom.Vec2{
			R: screenSize.R - f.location.R,
			C: f.desiredSize.C,
		},
	}

	if f.isUp {
		windowBounds.Position = geom.Vec2{
			R: 0,
			C: f.location.C,
		}
		windowBounds.Size.R = f.location.R
	}

	if f.desiredSize.R > 0 {
		windowBounds.Size.R = geom.Min(
			f.desiredSize.R,
			windowBounds.Size.R,
		)
	}

	bottomRight := windowBounds.BottomRight()
	if bottomRight.C >= screenSize.C {
		windowBounds.Position.C -= bottomRight.C - screenSize.C
	}
	if bottomRight.R >= screenSize.R {
		windowBounds.Position.R -= bottomRight.R - screenSize.R
	}

	// Default to full screen if we can't fit the match window
	if !f.isInline || windowBounds.Position.C < 0 ||
		windowBounds.Position.R < 0 {
		windowBounds.Size = screenSize
		windowBounds.Position = geom.Vec2{}

		if f.desiredSize.R > 0 {
			windowBounds.Size.R = geom.Min(
				f.desiredSize.R,
				windowBounds.Size.R,
			)
		}

		windowBounds.Position.R = 0

		if f.isUp {
			windowBounds.Position.R = screenSize.R - windowBounds.Size.R
		}
	}

	f.textInput.Width = windowBounds.Size.C - 2

	matchWindow := f.renderMatchWindow(windowBounds.Size)

	// We give the match window all the possible space it needs,
	// but it doesn't have to use it
	emptyRows := windowBounds.Size.R - matchWindow.Size().R
	if f.isUp && emptyRows > 0 {
		windowBounds.Position.R += emptyRows
	}

	image.Copy(windowBounds.Position, state.Image, matchWindow)
}
