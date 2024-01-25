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

func (f *Fuzzy) renderTable(common, active, inactive, prompt lipgloss.Style, options []Option) string {
	var rows [][]string

	for _, option := range options {
		rows = append(rows, option.Columns)
	}

	lines := strings.Split(table.New().
		Border(lipgloss.Border{}).
		StyleFunc(func(row, col int) lipgloss.Style {
			switch {
			case row == 0:
				return prompt
			case row == f.selected+1:
				return active
			default:
				return inactive
			}
		}).
		Headers(f.headers...).
		Width(common.GetWidth()+4).
		Rows(rows...).
		String(), "\n")

	// lipgloss adds some blank lines that we want to skip
	var output []string
	output = append(
		output,
		lines[1],
	)
	output = append(
		output,
		lines[3:len(lines)-1]...,
	)

	return strings.Join(output, "\n")
}

func (f *Fuzzy) renderOptions(common, prompt lipgloss.Style) string {
	inactive := common.Copy().
		Background(lipgloss.Color("#968C83")).
		Foreground(lipgloss.Color("#20111B"))
	active := common.Copy().
		Background(lipgloss.Color("#E8E3DF")).
		Foreground(lipgloss.Color("#20111B"))

	options := f.getOptions()
	if len(options) > 0 && len(options[0].Columns) > 0 {
		return f.renderTable(
			common,
			active,
			inactive,
			prompt,
			options,
		)
	}

	var lines []string

	// first, the options
	for i, match := range f.getOptions() {
		var rendered string
		if f.selected == i {
			rendered = active.Render("> " + match.Text)
		} else {
			rendered = inactive.Render("  " + match.Text)
		}

		if f.isUp {
			lines = append([]string{rendered}, lines...)
		} else {
			lines = append(lines, rendered)
		}
	}

	return lipgloss.JoinVertical(lipgloss.Left, lines...)
}

func (f *Fuzzy) renderPrompt(prompt lipgloss.Style, width int) string {
	style := prompt.Copy().Width(width)

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

	return style.Render(
		lipgloss.JoinHorizontal(
			lipgloss.Left,
			leftSide,
			lipgloss.PlaceHorizontal(
				width-lipgloss.Width(leftSide),
				lipgloss.Right,
				rightSide,
			),
		),
	)
}

func (f *Fuzzy) renderInline(prompt lipgloss.Style, state *tty.State) {
	common := f.render.NewStyle().
		Background(lipgloss.Color("#20111B")).
		Foreground(lipgloss.Color("#D5CCBA")).
		Width(30)

	f.textInput.Cursor.Style = f.render.NewStyle().
		Background(lipgloss.Color("#E8E3DF"))

	options := f.renderOptions(common, prompt)
	input := common.Render(f.textInput.View())
	output := lipgloss.JoinVertical(
		lipgloss.Left,
		input,
		f.renderPrompt(prompt, 30),
		options,
	)

	if f.isUp {
		output = lipgloss.JoinVertical(
			lipgloss.Left,
			options,
			f.renderPrompt(prompt, 30),
			input,
		)
	}

	offset := 0
	if f.isUp {
		offset += lipgloss.Height(output)
	}

	size := geom.Vec2{
		R: lipgloss.Height(output),
		C: lipgloss.Width(output),
	}

	f.render.RenderAt(
		state.Image,
		f.location.R-offset,
		geom.Clamp(f.location.C, 0, f.size.C-size.C),
		output,
	)
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

	prompt := f.render.NewStyle().
		Background(lipgloss.Color("#EAA549")).
		Foreground(lipgloss.Color("#20111B"))

	if f.isInline {
		f.renderInline(prompt, state)
		return
	}

	size := state.Image.Size()
	common := f.render.NewStyle().
		Background(lipgloss.Color("#20111B")).
		Foreground(lipgloss.Color("#D5CCBA")).
		Width(size.C)

	f.textInput.Cursor.Style = f.render.NewStyle().
		Background(lipgloss.Color("#E8E3DF"))

	options := f.renderOptions(common, prompt)
	input := common.Render(f.textInput.View())
	output := lipgloss.JoinVertical(
		lipgloss.Left,
		input,
		f.renderPrompt(prompt, size.C),
		options,
	)
	if f.isUp {
		output = lipgloss.JoinVertical(
			lipgloss.Left,
			options,
			f.renderPrompt(prompt, size.C),
			input,
		)
	}

	offset := 0
	if f.isUp {
		offset = size.R - lipgloss.Height(output)
	}

	f.render.RenderAt(
		state.Image,
		offset,
		0,
		output,
	)
}
