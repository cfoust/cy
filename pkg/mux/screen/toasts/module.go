package toasts

import (
	"context"
	"time"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type ToastLevel int

const (
	ToastLevelInfo ToastLevel = iota
	ToastLevelWarn
	ToastLevelError
)

type Toast struct {
	Message string
	Level   ToastLevel
}

type popToast struct{}

type Toaster struct {
	render *taro.Renderer

	toasts []Toast
}

var _ taro.Model = (*Toaster)(nil)

func (t *Toaster) Init() taro.Cmd {
	return nil
}

func (t *Toaster) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case popToast:
		if len(t.toasts) == 0 {
			return t, nil
		}

		t.toasts = t.toasts[1:]
		return t, nil
	case Toast:
		t.toasts = append(t.toasts, msg)
		return t, func() tea.Msg {
			time.Sleep(5 * time.Second)
			return popToast{}
		}
	}

	return t, nil
}

const TOAST_WIDTH = 45

func (t *Toaster) View(state *tty.State) {
	size := state.Image.Size()

	pos := geom.Vec2{
		R: 0,
		C: size.C - TOAST_WIDTH - 3,
	}

	border := t.render.NewStyle().
		Border(lipgloss.RoundedBorder()).
		BorderTop(true).
		BorderLeft(true).
		BorderRight(true).
		BorderBottom(true).
		Background(lipgloss.Color("0")).
		BorderBackground(lipgloss.Color("0")).
		Width(TOAST_WIDTH)

	var blocks []string
	var style lipgloss.Style
	for _, toast := range t.toasts {
		switch toast.Level {
		case ToastLevelError:
			style = border.
				BorderForeground(lipgloss.Color("9")).
				Foreground(lipgloss.Color("9"))
		case ToastLevelWarn:
			style = border.
				BorderForeground(lipgloss.Color("3")).
				Foreground(lipgloss.Color("3"))
		case ToastLevelInfo:
			style = border.
				BorderForeground(lipgloss.Color("14")).
				Foreground(lipgloss.Color("14"))
		}

		blocks = append(blocks, style.Render(toast.Message))
	}

	t.render.RenderAt(
		state.Image,
		pos.R, pos.C,
		lipgloss.JoinVertical(lipgloss.Left, blocks...),
	)
}

func New(ctx context.Context) *taro.Program {
	return taro.New(ctx, &Toaster{
		render: taro.NewRenderer(),
	})
}
