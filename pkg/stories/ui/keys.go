package ui

import (
	"context"
	"time"

	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

const KEY_COLUMNS = 10

type Keys struct {
	render  *taro.Renderer
	presses []keyPress
}

type keyPress struct {
	stamp time.Time
	key   string
}

type popKey struct{}

type refresh struct{}

var _ taro.Model = (*Keys)(nil)

func (k *Keys) wait() tea.Msg {
	time.Sleep(50 * time.Millisecond)
	return refresh{}
}

func (k *Keys) Init() tea.Cmd {
	return k.wait
}

func (k *Keys) View(state *tty.State) {
	size := state.Image.Size()

	bgColor := lipgloss.Color("7")
	border := k.render.NewStyle().
		Border(lipgloss.NormalBorder()).
		BorderForeground(bgColor).
		BorderTop(true).
		BorderLeft(true).
		BorderRight(true).
		BorderBottom(true).
		Width(size.C).
		Height(size.R).
		Background(bgColor).
		Render("")

	k.render.RenderAt(
		state.Image,
		-1, -2,
		border,
	)

	presses := k.presses
	last := len(k.presses) - 1
	for i := last; i >= 0; i-- {
		press := presses[i]
		style := k.render.NewStyle().
			Width(size.C - 1).
			Background(lipgloss.Color("#EAA549")).
			Foreground(lipgloss.Color("#20111B"))

		delta := time.Now().Sub(press.stamp)

		if delta > 400*time.Millisecond || i != last {
			style = k.render.NewStyle().
				Foreground(lipgloss.Color("0")).
				Background(bgColor)
		}

		k.render.RenderAt(
			state.Image,
			last-i,
			0,
			style.Render(press.key),
		)
	}
}

func (k *Keys) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case refresh:
		return k, k.wait
	case popKey:
		if len(k.presses) == 0 {
			return k, nil
		}

		k.presses = k.presses[1:]
		return k, nil
	case taro.KeyMsg:
		k.presses = append(k.presses, keyPress{
			stamp: time.Now(),
			key:   msg.String(),
		})
		return k, func() tea.Msg {
			time.Sleep(10 * time.Second)
			return popKey{}
		}
	}
	return k, nil
}

func NewKeys(
	ctx context.Context,
) *taro.Program {
	program := taro.New(ctx, &Keys{
		render: taro.NewRenderer(),
	})

	return program
}
