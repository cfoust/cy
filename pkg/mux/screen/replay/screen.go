package replay

import (
	"context"
	"time"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/latte"
	"github.com/cfoust/cy/pkg/sessions"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type Replay struct {
	render *latte.Renderer

	// the size of the terminal
	terminal emu.Terminal

	// the offset of the displayed event in `events`
	index  int
	events []sessions.Event

	// the location of the cursor relative to the top of the terminal's
	// lines including the scrollback buffer
	cursor geom.Size

	offset    int
	maxOffset int
	numLines  int
}

var _ latte.Model = (*Replay)(nil)

func (r *Replay) quit() (latte.Model, tea.Cmd) {
	return r, tea.Quit
}

func (r *Replay) setOffset(offset int) {
	r.offset = geom.Clamp(offset, 0, r.maxOffset)
}

// Move the terminal from event index `from` to `to`.
func (r *Replay) setIndex(index int) {
	numEvents := len(r.events)
	// Allow for negative indices from end of stream
	if index < 0 {
		index = geom.Clamp(numEvents+index, 0, numEvents-1)
	}

	from := geom.Clamp(r.index, 0, numEvents-1)
	to := geom.Clamp(index, 0, numEvents-1)

	if from == to {
		return
	}

	// don't allow looking at scrollback when moving in time
	r.offset = 0

	origin := from + 1
	if from > to {
		r.terminal = emu.New()
		origin = 0
	}

	for i := origin; i <= to; i++ {
		event := r.events[i]
		switch e := event.Data.(type) {
		case sessions.OutputEvent:
			r.terminal.Write(e.Bytes)
		case sessions.ResizeEvent:
			r.terminal.Resize(
				e.Columns,
				e.Rows,
			)
		}
		r.maxOffset = len(r.terminal.History())
	}

	r.index = to
}

func (r *Replay) Init() tea.Cmd {
	return nil
}

func (r *Replay) Update(msg tea.Msg) (latte.Model, tea.Cmd) {
	_, rows := r.terminal.Size()

	switch msg := msg.(type) {
	case latte.KeyMsg:
		switch msg.Type {
		case latte.KeyRunes:
			switch msg.String() {
			case "g":
				r.setIndex(0)
				return r, nil
			case "G":
				r.setIndex(-1)
				return r, nil
			case "q":
				return r.quit()
			}
		case latte.KeyEsc, latte.KeyCtrlC:
			return r.quit()
		case latte.KeyLeft:
			r.setIndex(r.index - 1)
			return r, nil
		case latte.KeyRight:
			r.setIndex(r.index + 1)
			return r, nil
		case latte.KeyCtrlU:
			r.setOffset(r.offset + (rows / 2))
		case latte.KeyCtrlD:
			r.setOffset(r.offset - (rows / 2))
		case latte.KeyUp:
			r.setOffset(r.offset + 1)
		case latte.KeyDown:
			r.setOffset(r.offset - 1)
		}
	}

	return r, nil
}

func (r *Replay) View(state *tty.State) {
	termCols, termRows := r.terminal.Size()
	screen := r.terminal.Screen()
	history := r.terminal.History()

	size := state.Image.Size()

	state.CursorVisible = false

	// TODO(cfoust): 08/10/23 indicate smaller/bigger size somehow
	var rowIndex int
	for row := 0; row < geom.Min(termRows, size.R); row++ {
		rowIndex = row - r.offset
		for col := 0; col < geom.Min(termCols, size.C); col++ {
			if rowIndex < 0 {
				state.Image[row][col] = history[len(history)+rowIndex][col]
			} else {
				state.Image[row][col] = screen[rowIndex][col]
			}
		}
	}

	//state.Cursor = termState.Cursor

	basic := r.render.NewStyle().
		Foreground(lipgloss.Color("#D5CCBA")).
		Width(size.C).
		Align(lipgloss.Right)

	index := r.index
	if index < 0 || index >= len(r.events) || len(r.events) == 0 {
		r.render.RenderAt(state, 0, 0, basic.Render("???"))
		return
	}

	r.render.RenderAt(
		state,
		0,
		0,
		basic.Render(r.events[index].Stamp.Format(time.RFC1123)),
	)
}

func New(ctx context.Context, recorder *sessions.Recorder) *latte.Program {
	events := recorder.Events()
	m := &Replay{
		render:   latte.NewRenderer(),
		events:   events,
		terminal: emu.New(),
	}
	m.setIndex(-1)
	return latte.New(ctx, m)
}
