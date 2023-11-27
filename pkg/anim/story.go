package anim

import (
	"context"
	_ "embed"

	"github.com/cfoust/cy/pkg/frames"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/glamour"
)

//go:embed example.md
var EXAMPLE_MD string

type Markdown struct {
	render *taro.Renderer
}

var _ taro.Model = (*Markdown)(nil)

func (s *Markdown) Init() tea.Cmd {
	return nil
}

func (s *Markdown) View(state *tty.State) {
	text, _ := glamour.Render(EXAMPLE_MD, "dark")
	s.render.RenderAt(
		state.Image,
		0,
		0,
		text,
	)
}

func (s *Markdown) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	return s, nil
}

func createInitial(size geom.Size) image.Image {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	innerLayers := screen.NewLayers()
	innerLayers.NewLayer(
		ctx,
		taro.New(ctx, &Markdown{
			render: taro.NewRenderer(),
		}),
		screen.PositionTop,
		screen.WithOpaque,
		screen.WithInteractive,
	)
	margins := screen.NewMargins(ctx, innerLayers)

	outerLayers := screen.NewLayers()
	frame := frames.NewFramer(ctx, frames.RandomFrame())
	outerLayers.NewLayer(
		ctx,
		frame,
		screen.PositionTop,
	)

	outerLayers.NewLayer(
		ctx,
		margins,
		screen.PositionTop,
		screen.WithInteractive,
		screen.WithOpaque,
	)

	outerLayers.Resize(size)

	return outerLayers.State().Image
}

type Story struct {
	util.Lifetime
	render   *taro.Renderer
	animator *taro.Program
	creator  Creator
}

var _ taro.Model = (*Story)(nil)

func (s *Story) Init() tea.Cmd {
	return taro.WaitScreens(s.animator.Ctx(), s.animator)
}

func (s *Story) View(state *tty.State) {
	tty.Copy(geom.Size{}, state, s.animator.State())
}

func (s *Story) initialize(size geom.Size) {
	if s.animator != nil {
		s.animator.Cancel()
	}

	s.animator = NewAnimator(
		s.Ctx(),
		s.creator(),
		createInitial(size),
		23,
	)
}

func (s *Story) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		s.initialize(geom.Size{
			R: msg.Height,
			C: msg.Width,
		})
		return s, taro.WaitScreens(s.animator.Ctx(), s.animator)
	case taro.ScreenUpdate:
		return s, taro.WaitScreens(s.animator.Ctx(), s.animator)
	}

	return s, nil
}

func NewStory(
	ctx context.Context,
	creator Creator,
) *taro.Program {
	story := &Story{
		Lifetime: util.NewLifetime(ctx),
		render:   taro.NewRenderer(),
		creator:  creator,
	}

	story.initialize(geom.DEFAULT_SIZE)

	return taro.New(ctx, story)
}
