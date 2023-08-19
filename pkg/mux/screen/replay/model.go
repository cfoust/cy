package replay

import (
	"time"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/util"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type model struct {
	renderer *lipgloss.Renderer
	lifetime *util.Lifetime
	size     screen.Size
	events   []sessions.Event

	index int
}

var _ tea.Model = (*model)(nil)

func (m *model) Init() tea.Cmd {
	return nil
}

func (m *model) quit() (tea.Model, tea.Cmd) {
	m.lifetime.Cancel()
	return m, tea.Quit
}

func (m *model) setIndex(index int) {
	numEvents := len(m.events)
	// Allow for negative indices from end of stream
	if index < 0 {
		index = geom.Clamp(numEvents+index, 0, numEvents-1)
	}

	m.index = geom.Clamp(index, 0, numEvents-1)
}

func (m *model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.size = screen.Size{
			R: msg.Height,
			C: msg.Width,
		}
		return m, nil
	case tea.KeyMsg:
		switch msg.Type {
		case tea.KeyRunes:
			switch msg.String() {
			case "q":
				return m.quit()
			}
		case tea.KeyEsc, tea.KeyCtrlC:
			return m.quit()
		case tea.KeyLeft:
			m.setIndex(m.index - 1)
			return m, nil
		case tea.KeyRight:
			m.setIndex(m.index + 1)
			return m, nil
		}
	}

	return m, nil
}

func (m *model) View() string {
	width := m.size.C

	basic := m.renderer.NewStyle().
		Foreground(lipgloss.Color("#D5CCBA")).
		Width(width).
		Align(lipgloss.Right)

	index := m.index
	if index < 0 || index >= len(m.events) || len(m.events) == 0 {
		return basic.Render("???")
	}

	event := m.events[index]
	stamp := basic.Render(event.Stamp.Format(time.RFC1123))

	return stamp
}
