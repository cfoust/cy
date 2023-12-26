package ui

import (
	"context"
	"time"

	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
)

type Keys struct {
	render  *taro.Renderer
	presses []keyPress
}

type keyPress struct {
	stamp time.Time
	key   string
}

type popKey struct{}

var _ taro.Model = (*Keys)(nil)

func (k *Keys) Init() tea.Cmd {
	return nil
}

func (k *Keys) View(state *tty.State) {
	k.render.RenderAt(
		state.Image,
		0,
		0,
		"keys:",
	)

	for i, press := range k.presses {
		k.render.RenderAt(
			state.Image,
			i+1,
			0,
			press.key,
		)
	}
}

func (k *Keys) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	switch msg := msg.(type) {
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
