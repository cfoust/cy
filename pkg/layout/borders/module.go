package borders

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	L "github.com/cfoust/cy/pkg/layout"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/style"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/charmbracelet/lipgloss"
	"github.com/sasha-s/go-deadlock"
)

type Borders struct {
	deadlock.RWMutex
	*mux.UpdatePublisher
	render      *taro.Renderer
	screen      mux.Screen
	size        geom.Size
	inner       geom.Rect
	borderStyle *style.Border

	title, titleBottom string
}

var _ mux.Screen = (*Borders)(nil)
var _ L.Reusable = (*Borders)(nil)

func (l *Borders) Apply(node L.NodeType) (bool, error) {
	config, ok := node.(L.BorderType)
	if !ok {
		return false, nil
	}

	l.Lock()
	defer l.Unlock()

	l.borderStyle = config.Border

	if config.Title != nil {
		l.title = *config.Title
	}

	if config.TitleBottom != nil {
		l.titleBottom = *config.TitleBottom
	}

	return true, nil
}

func (l *Borders) Kill() {
	l.RLock()
	screen := l.screen
	l.RUnlock()

	screen.Kill()
}

func (l *Borders) State() *tty.State {
	l.RLock()
	inner := l.inner
	size := l.size
	borderStyle := l.borderStyle
	title := l.title
	titleBottom := l.titleBottom
	l.RUnlock()

	innerState := l.screen.State()
	state := tty.New(size)

	tty.Copy(inner.Position, state, innerState)

	boxText := l.render.NewStyle().
		Border(borderStyle.Border).
		BorderForeground(lipgloss.Color("7")).
		BorderTop(true).
		BorderLeft(true).
		BorderRight(true).
		BorderBottom(true).
		Width(inner.Size.C).
		Height(inner.Size.R).
		Render("")

	l.render.RenderAt(state.Image, 0, 0, boxText)

	if len(title) > 0 {
		l.render.RenderAt(
			state.Image,
			0, 1,
			l.render.NewStyle().
				MaxWidth(inner.Size.C).
				Render(title),
		)
	}

	if len(titleBottom) > 0 {
		l.render.RenderAt(
			state.Image,
			size.R-1, 1,
			l.render.NewStyle().
				MaxWidth(inner.Size.C).
				Render(titleBottom),
		)
	}

	return state
}

func (l *Borders) Send(msg mux.Msg) {
	l.RLock()
	inner := l.inner
	l.RUnlock()
	l.screen.Send(taro.TranslateMouseMessage(
		msg,
		-inner.Position.C,
		-inner.Position.R,
	))
}

func (l *Borders) Size() geom.Size {
	l.RLock()
	defer l.RUnlock()
	return l.size
}

func (l *Borders) poll(ctx context.Context) {
	updates := l.screen.Subscribe(ctx)

	for {
		select {
		case <-ctx.Done():
			return
		case event := <-updates.Recv():
			if _, ok := event.(L.NodeChangeEvent); ok {
				continue
			}
			l.Publish(event)
		}
	}
}

func (l *Borders) recalculate() error {
	l.RLock()
	size := l.size
	l.RUnlock()

	inner := geom.Rect{
		Position: geom.UnitVec2,
		Size: geom.Vec2{
			R: geom.Max(1, size.R-2),
			C: geom.Max(1, size.C-2),
		},
	}

	l.Lock()
	l.inner = inner
	l.Unlock()

	err := l.screen.Resize(inner.Size)
	if err != nil {
		return err
	}

	return nil
}

func (l *Borders) Resize(size geom.Size) error {
	l.Lock()
	l.size = size
	l.Unlock()
	return l.recalculate()
}

func New(ctx context.Context, screen mux.Screen) *Borders {
	borders := &Borders{
		UpdatePublisher: mux.NewPublisher(),
		size:            geom.DEFAULT_SIZE,
		screen:          screen,
		render:          taro.NewRenderer(),
	}

	go borders.poll(ctx)

	return borders
}
