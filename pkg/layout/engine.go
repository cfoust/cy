package layout

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"

	"github.com/sasha-s/go-deadlock"
)

type LayoutEngine struct {
	deadlock.RWMutex
	*mux.UpdatePublisher
	layout Layout
}

var _ mux.Screen = (*LayoutEngine)(nil)

func (l *LayoutEngine) Kill() {
}

func (l *LayoutEngine) State() *tty.State {
	state := tty.New(geom.DEFAULT_SIZE)
	return state
}

func (l *LayoutEngine) Send(msg mux.Msg) {
}

func (l *LayoutEngine) Resize(size geom.Size) error {
	return nil
}

func (l *LayoutEngine) Set(layout Layout) error {
	err := validateTree(layout.root)
	if err != nil {
		return err
	}

	l.Lock()
	l.layout = layout
	l.Unlock()
	return nil
}

func (l *LayoutEngine) Get() Layout {
	l.RLock()
	layout := l.layout
	l.RUnlock()
	return layout
}

func NewLayoutEngine(ctx context.Context) *LayoutEngine {
	engine := &LayoutEngine{
		UpdatePublisher: mux.NewPublisher(),
	}

	return engine
}
