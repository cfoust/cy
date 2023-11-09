package fuzzy

import (
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"

	"github.com/charmbracelet/lipgloss"
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
	previewPos := geom.Vec2{
		R: (size.R / 2) - (previewSize.R / 2),
		C: (size.C / 2) - (previewSize.C / 2),
	}

	state.Image.Clear(geom.Rect{
		R: previewPos.R,
		C: previewPos.C,
		H: previewSize.R,
		W: previewSize.C,
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

func (f *Fuzzy) View(state *tty.State) {
	tty.Copy(geom.Vec2{}, state, f.anim.State())

	if f.haveMoved {
		f.renderPreview(state)
	}

	basic := f.render.NewStyle().
		Background(lipgloss.Color("#20111B")).
		Foreground(lipgloss.Color("#D5CCBA")).
		Width(20)

	inactive := basic.Copy().Background(lipgloss.Color("#968C83"))
	active := basic.Copy().
		Background(lipgloss.Color("#EAA549")).
		Foreground(lipgloss.Color("#20111B"))

	var lines []string

	isInverted := f.isInverted()

	// first, the options
	for i, match := range f.getOptions() {
		var rendered string
		if f.selected == i {
			rendered = active.Render(match.Text)
		} else {
			rendered = inactive.Render(match.Text)
		}

		if isInverted {
			lines = append([]string{rendered}, lines...)
		} else {
			lines = append(lines, rendered)
		}
	}

	// then the text input
	input := basic.Render(f.textInput.View())
	if isInverted {
		lines = append(lines, input)
	} else {
		lines = append([]string{input}, lines...)
	}

	f.textInput.Cursor.Style = f.render.NewStyle().
		Background(lipgloss.Color("#EAA549"))

	output := lipgloss.JoinVertical(lipgloss.Left, lines...)

	offset := 0
	if isInverted {
		offset += lipgloss.Height(output) - 1
	}

	f.render.RenderAt(
		state.Image,
		geom.Max(f.location.R-offset, 0),
		f.location.C,
		output,
	)

	// the text input provides its own cursor
	state.CursorVisible = false
}
