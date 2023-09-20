package replay

import (
	"context"
	"fmt"
	"time"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type Replay struct {
	render *taro.Renderer

	// the size of the terminal
	terminal emu.Terminal

	// the offset of the displayed event in `events`
	index  int
	events []sessions.Event

	// the location of the cursor relative to the top of the terminal's
	// lines including the scrollback buffer
	// TODO(cfoust): 09/20/23
	cursor geom.Size

	offset    int
	maxOffset int
	numLines  int
}

var _ taro.Model = (*Replay)(nil)

func (r *Replay) quit() (taro.Model, tea.Cmd) {
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
		switch e := event.Message.(type) {
		case P.OutputMessage:
			r.terminal.Write(e.Data)
		case P.SizeMessage:
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

func (r *Replay) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	_, rows := r.terminal.Size()

	switch msg := msg.(type) {
	case taro.KeyMsg:
		switch msg.Type {
		case taro.KeyRunes:
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
		case taro.KeyEsc, taro.KeyCtrlC:
			return r.quit()
		case taro.KeyLeft:
			r.setIndex(r.index - 1)
			return r, nil
		case taro.KeyRight:
			r.setIndex(r.index + 1)
			return r, nil
		case taro.KeyCtrlU:
			r.setOffset(r.offset + (rows / 2))
		case taro.KeyCtrlD:
			r.setOffset(r.offset - (rows / 2))
		case taro.KeyUp:
			r.setOffset(r.offset + 1)
		case taro.KeyDown:
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
		Background(lipgloss.Color("#000000")).
		Align(lipgloss.Right)

	index := r.index
	if index < 0 || index >= len(r.events) || len(r.events) == 0 {
		r.render.RenderAt(state, 0, 0, basic.Render("???"))
		return
	}

	headline := r.events[index].Stamp.Format(time.RFC1123)

	if r.offset > 0 {
		headline = fmt.Sprintf(
			"[%d/%d]",
			r.offset,
			r.maxOffset,
		)
	}

	r.render.RenderAt(
		state,
		0,
		0,
		r.render.PlaceHorizontal(
			size.C,
			lipgloss.Right,
			basic.Render(headline),
		),
	)
}

func New(ctx context.Context, recorder *sessions.Recorder) *taro.Program {
	events := recorder.Events()
	m := &Replay{
		render:   taro.NewRenderer(),
		events:   events,
		terminal: emu.New(),
	}
	m.setIndex(-1)
	return taro.New(ctx, m)
}
