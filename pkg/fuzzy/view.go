package fuzzy

import (
	"fmt"
	"strings"

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

	switch data := option.Preview.(type) {
	case nodePreview:
		if f.isAttached {
			state := f.client.State()
			preview = image.New(state.Image.Size())
			image.Copy(geom.Vec2{}, preview, state.Image)

			// draw a ghost cursor
			cursor := state.Cursor
			if state.CursorVisible {
				preview[cursor.Y][cursor.X].BG = 8
			}
			return
		}

		preview = image.New(geom.DEFAULT_SIZE)
		f.render.RenderAt(
			preview,
			0, 0,
			lipgloss.Place(
				geom.DEFAULT_SIZE.C,
				geom.DEFAULT_SIZE.R,
				lipgloss.Center, lipgloss.Center,
				"attaching to pane",
			),
		)
		return
	case replayPreview:
		if f.replay == nil {
			return nil
		}
		state := f.replay.State()
		preview = image.New(state.Image.Size())
		image.Copy(geom.Vec2{}, preview, state.Image)

		// draw a ghost cursor
		cursor := state.Cursor
		if state.CursorVisible {
			preview[cursor.Y][cursor.X].BG = 8
		}
		return
	case textPreview:
		preview = image.New(geom.DEFAULT_SIZE)
		f.render.RenderAt(
			preview,
			0, 0,
			f.render.NewStyle().
				MaxWidth(geom.DEFAULT_SIZE.C).
				MaxHeight(geom.DEFAULT_SIZE.R).
				Render(data.Text),
		)
		return
	}

	return nil
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

	border := f.render.NewStyle().
		Border(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("#874BFD")).
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

func (f *Fuzzy) renderOptions(common, prompt lipgloss.Style, maxOptions int) string {
	rowStyle := common.Copy().
		MaxHeight(1)
	inactive := rowStyle.Copy().
		Background(lipgloss.Color("#968C83")).
		Foreground(lipgloss.Color("#20111B"))
	active := rowStyle.Copy().
		Background(lipgloss.Color("#E8E3DF")).
		Foreground(lipgloss.Color("#20111B"))

	options := f.getOptions()
	if len(options) == 0 {
		return ""
	}

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

		rows = append(rows, columns)
	}

	lines := strings.Split(table.New().
		Border(lipgloss.Border{}).
		StyleFunc(func(row, col int) lipgloss.Style {
			switch {
			case row == 0:
				return prompt
			case row == (f.selected-windowOffset)+1:
				return active
			default:
				return inactive
			}
		}).
		Headers(headers...).
		Width(common.GetWidth()+4).
		Rows(rows...).
		String(), "\n")

	if len(lines) < 4 {
		return ""
	}

	// lipgloss adds some blank lines that we want to skip
	var output []string
	if haveHeaders {
		output = append(
			output,
			lines[1],
		)
	}
	output = append(
		output,
		lines[3:len(lines)-1]...,
	)

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
	commonStyle := f.render.NewStyle().Width(size.C)
	promptStyle := commonStyle.Copy().
		Background(lipgloss.Color("#EAA549")).
		Foreground(lipgloss.Color("#20111B"))

	options := f.renderOptions(commonStyle, promptStyle, size.R-2)

	textInput := commonStyle.Copy().
		Background(lipgloss.Color("#20111B")).
		Foreground(lipgloss.Color("#D5CCBA")).
		Render(f.textInput.View())

	prompt := f.renderPrompt(promptStyle)

	output := lipgloss.JoinVertical(
		lipgloss.Left,
		textInput,
		prompt,
		options,
	)
	if f.isUp {
		output = lipgloss.JoinVertical(
			lipgloss.Left,
			options,
			prompt,
			textInput,
		)
	}

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
	}

	if f.haveMoved {
		f.renderPreview(state)
	}

	// the text input provides its own cursor
	state.CursorVisible = false

	f.textInput.Cursor.Style = f.render.NewStyle().
		Background(lipgloss.Color("#E8E3DF"))

	screenSize := state.Image.Size()

	windowBounds := geom.Rect{
		Position: f.location,
		Size: geom.Vec2{
			R: screenSize.R - f.location.R,
			C: 40,
		},
	}

	if f.isUp {
		windowBounds.Position = geom.Vec2{
			R: 0,
			C: f.location.C,
		}
		windowBounds.Size.R = f.location.R
	}

	bottomRight := windowBounds.BottomRight()
	if bottomRight.C >= screenSize.C {
		windowBounds.Position.C -= bottomRight.C - screenSize.C
	}
	if bottomRight.R >= screenSize.R {
		windowBounds.Position.R -= bottomRight.R - screenSize.R
	}

	// Default to full screen if we can't fit the match window
	if !f.isInline || windowBounds.Position.C < 0 || windowBounds.Position.R < 0 {
		windowBounds.Size = screenSize
		windowBounds.Position = geom.Vec2{}
	}

	matchWindow := f.renderMatchWindow(windowBounds.Size)

	// We give the match window all the possible space it needs,
	// but it doesn't have to use it
	emptyRows := windowBounds.Size.R - matchWindow.Size().R
	if f.isUp && emptyRows > 0 {
		windowBounds.Position.R += emptyRows
	}

	image.Copy(windowBounds.Position, state.Image, matchWindow)
}
