package replay

import (
	"time"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/mux/stream"
	"github.com/cfoust/cy/pkg/util"

	"github.com/charmbracelet/bubbles/stopwatch"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type model struct {
	stopwatch stopwatch.Model
	renderer  *lipgloss.Renderer
	lifetime  *util.Lifetime
	size      screen.Size
	events    []stream.Event

	index int
}

var _ tea.Model = (*model)(nil)

func (m *model) Init() tea.Cmd {
	return m.stopwatch.Init()
}

func (m *model) quit() (tea.Model, tea.Cmd) {
	m.lifetime.Cancel()
	return m, tea.Quit
}

func (m *model) setIndex(index int) {
	m.index = geom.Clamp(index, 0, len(m.events)-1)
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
		case tea.KeySpace:
			return m, m.stopwatch.Toggle()
		case tea.KeyLeft:
			m.setIndex(m.index - 1)
			return m, nil
		case tea.KeyRight:
			m.setIndex(m.index + 1)
			return m, nil
		}
	}

	var cmd tea.Cmd
	m.stopwatch, cmd = m.stopwatch.Update(msg)
	return m, cmd
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

	statusBarStyle := m.renderer.NewStyle().
		Foreground(lipgloss.AdaptiveColor{Light: "#343433", Dark: "#C1C6B2"}).
		Background(lipgloss.AdaptiveColor{Light: "#D9DCCF", Dark: "#353533"})

	statusText := m.renderer.NewStyle().Inherit(statusBarStyle)

	statusKey := m.renderer.NewStyle().
		Inherit(statusBarStyle).
		Foreground(lipgloss.Color("#FFFDF5")).
		Background(lipgloss.Color("#FF5F87")).
		Padding(0, 1).
		MarginRight(1).
		Render("⏵")

	bar := statusText.Copy().
		Width(width - lipgloss.Width(statusKey)).
		Render(m.stopwatch.View())

	return lipgloss.JoinVertical(
		lipgloss.Left,
		stamp,
		lipgloss.Place(
			m.size.C,
			m.size.R-1,
			lipgloss.Left,
			lipgloss.Bottom,
			lipgloss.JoinHorizontal(lipgloss.Top, statusKey, bar),
		),
	)
}
