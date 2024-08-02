package layout

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
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
}

var _ mux.Screen = (*Borders)(nil)
var _ reusable = (*Borders)(nil)

func (l *Borders) reuse(node NodeType) (bool, error) {
	config, ok := node.(BorderType)
	if !ok {
		return false, nil
	}

	l.borderStyle = config.Border
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
			if _, ok := event.(nodeChangeEvent); ok {
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

func (l *LayoutEngine) createBorders(
	node *screenNode,
	config BorderType,
) error {
	innerNode, err := l.createNode(
		node.Ctx(),
		config.Node,
	)
	if err != nil {
		return err
	}

	borders := NewBorders(
		node.Ctx(),
		innerNode.Screen,
	)
	borders.borderStyle = config.Border
	node.Screen = borders
	node.Children = []*screenNode{innerNode}
	return nil
}

func NewBorders(ctx context.Context, screen mux.Screen) *Borders {
	borders := &Borders{
		UpdatePublisher: mux.NewPublisher(),
		size:            geom.DEFAULT_SIZE,
		screen:          screen,
		render:          taro.NewRenderer(),
	}

	go borders.poll(ctx)

	return borders
}
