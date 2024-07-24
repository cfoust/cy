package layout

import (
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
)

type LayoutEngine struct {
	*mux.UpdatePublisher
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

func (l *LayoutEngine) Set(layout LayoutType) error {
	return nil
}
