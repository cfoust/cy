package copy

import (
	"context"
	"fmt"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/util"
	"github.com/cfoust/cy/pkg/mux/screen"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/muesli/termenv"
)

type CopyMode struct {
	util.Lifetime
	*screen.Trigger
	history []emu.Line
	overlay *screen.Tea
	model   *model
}

var _ screen.Screen = (*CopyMode)(nil)

func (c *CopyMode) Render(size screen.Size) *tty.State {
	out := image.New(size)
	for row := 0; row < size.R; row++ {
		srcRow := c.model.offset + row
		if srcRow >= len(c.history) {
			break
		}

		for col := 0; col < size.C; col++ {
			if col >= len(c.history[srcRow]) {
				break
			}

			out[row][col] = c.history[srcRow][col]
		}
	}

	image.Compose(geom.Vec2{}, out, c.overlay.State().Image)

	state := tty.New(size)
	state.Image = out
	state.CursorVisible = false
	return state
}

func (c *CopyMode) Write(data []byte) (n int, err error) {
	return c.overlay.Write(data)
}

func (c *CopyMode) poll(ctx context.Context) {
	updates := c.overlay.Updates()
	defer updates.Done()
	for {
		select {
		case <-ctx.Done():
			return
		case <-updates.Recv():
			c.Rerender()
		}
	}
}

func (c *CopyMode) Resize(size screen.Size) error {
	err := c.overlay.Resize(size)
	if err != nil {
		return err
	}

	return c.Trigger.Resize(size)
}

type model struct {
	renderer  *lipgloss.Renderer
	lifetime  *util.Lifetime
	offset    int
	maxOffset int
	history   int
	size      screen.Size
}

var _ tea.Model = (*model)(nil)

func (m *model) Init() tea.Cmd {
	return nil
}

func (m *model) setOffset(offset int) {
	m.offset = geom.Clamp(offset, 0, m.maxOffset)
}

func getMaxOffset(rows, history int) int {
	return geom.Max(history-rows, 0)
}

func (m *model) quit() (tea.Model, tea.Cmd) {
	m.lifetime.Cancel()
	return m, tea.Quit
}

func (m *model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.size = screen.Size{
			R: msg.Height,
			C: msg.Width,
		}
		m.maxOffset = getMaxOffset(m.size.R, m.history)
		m.setOffset(m.offset) // trigger clamp
		return m, nil
	case tea.KeyMsg:
		switch msg.Type {
		case tea.KeyRunes:
			switch msg.String() {
			case "q":
				return m.quit()
			case "j":
				m.setOffset(m.offset + 1)
			case "k":
				m.setOffset(m.offset - 1)
			}
		case tea.KeyEsc, tea.KeyCtrlC:
			return m.quit()
		case tea.KeyUp:
			m.setOffset(m.offset - 1)
			return m, nil
		case tea.KeyDown:
			m.setOffset(m.offset + 1)
			return m, nil
		case tea.KeyCtrlU:
			m.setOffset(m.offset - (m.size.R / 2))
		case tea.KeyCtrlD:
			m.setOffset(m.offset + (m.size.R / 2))
		case tea.KeyCtrlB:
			m.setOffset(m.offset - m.size.R)
		case tea.KeyCtrlF:
			m.setOffset(m.offset + m.size.R)
		}
	}

	return m, nil
}

func (m *model) View() string {
	basic := m.renderer.NewStyle().
		Foreground(lipgloss.Color("#D5CCBA")).
		Width(m.size.C).
		Align(lipgloss.Right)

	return basic.Render(fmt.Sprintf(
		"[%d/%d]",
		m.maxOffset-m.offset,
		m.maxOffset,
	))
}

func New(
	ctx context.Context,
	info screen.RenderContext,
	history []emu.Line,
	size screen.Size,
) *CopyMode {
	lifetime := util.NewLifetime(ctx)

	maxOffset := getMaxOffset(size.R, len(history))
	m := &model{
		lifetime:  &lifetime,
		offset:    maxOffset,
		maxOffset: maxOffset,
		history:   len(history),
		renderer: lipgloss.NewRenderer(
			nil,
			termenv.WithProfile(info.Colors),
		),
	}

	overlay := screen.NewTea(
		lifetime.Ctx(),
		m,
		info,
		size,
	)

	c := &CopyMode{
		Lifetime: lifetime,
		history:  history,
		overlay:  overlay,
		model:    m,
	}
	c.Trigger = screen.NewTrigger(c)

	go c.poll(lifetime.Ctx())

	return c
}
