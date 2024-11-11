package pane

import (
	"context"

	"github.com/cfoust/cy/pkg/anim"
	"github.com/cfoust/cy/pkg/anim/static"
	"github.com/cfoust/cy/pkg/frames"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type Static struct {
	util.Lifetime
	render        *taro.Renderer
	shouldAnimate bool
	message       string

	size    geom.Vec2
	watcher *taro.ScreenWatcher
	bg      mux.Screen
}

var _ taro.Model = (*Static)(nil)

func (s *Static) resetScreen(size geom.Vec2) taro.Cmd {
	if s.size == size && s.watcher != nil {
		return s.watcher.Wait()
	}

	if s.bg != nil {
		s.bg.Kill()
	}
	if s.watcher != nil {
		s.watcher.Cancel()
	}

	var bg mux.Screen
	initial := image.New(size)
	if s.shouldAnimate {
		bg = anim.NewAnimator(
			s.Ctx(),
			&static.Static{},
			initial,
			1,
		)
	} else {
		bg = frames.NewFramer(
			s.Ctx(),
			func(state image.Image) {
				image.Copy(
					geom.Vec2{},
					state,
					initial,
				)
			},
		)
	}

	bg.Resize(size)

	s.size = size
	s.bg = bg
	s.watcher = taro.NewWatcher(s.Ctx(), bg)
	return s.watcher.Wait()
}

func (s *Static) Init() taro.Cmd {
	return s.resetScreen(geom.DEFAULT_SIZE)
}

func (s *Static) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		return s, s.resetScreen(geom.Size{
			R: msg.Height,
			C: msg.Width,
		})
	case taro.ScreenUpdate:
		return s, msg.Wait()
	}

	return s, nil
}

func (s *Static) View(state *tty.State) {
	size := state.Image.Size()
	state.CursorVisible = false

	bg := s.bg.State().Image
	bgSize := bg.Size()
	image.Copy(size.Center(bgSize), state.Image, bg)

	boxStyle := s.render.NewStyle().
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("7")).
		BorderTop(true).
		BorderLeft(true).
		BorderRight(true).
		BorderBottom(true)

	boxText := boxStyle.Render(s.message)
	boxSize := taro.GetSize(boxText)

	box := image.New(boxSize)
	s.render.RenderAt(
		box,
		0,
		0,
		boxText,
	)

	image.Copy(size.Center(boxSize), state.Image, box)
}

func NewStatic(
	ctx context.Context,
	shouldAnimate bool,
	message string,
) *taro.Program {
	lifetime := util.NewLifetime(ctx)
	render := taro.NewRenderer()

	return taro.New(ctx, &Static{
		Lifetime:      lifetime,
		render:        render,
		message:       message,
		shouldAnimate: shouldAnimate,
	})
}
