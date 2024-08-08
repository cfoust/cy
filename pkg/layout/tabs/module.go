package tabs

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	L "github.com/cfoust/cy/pkg/layout"
	"github.com/cfoust/cy/pkg/mux"

	"github.com/sasha-s/go-deadlock"
)

type Tabs struct {
	deadlock.RWMutex
	*mux.UpdatePublisher
}

var _ mux.Screen = (*Tabs)(nil)
var _ L.Reusable = (*Tabs)(nil)

func (t *Tabs) Kill() {
}

func (t *Tabs) State() *tty.State {
	return tty.New(geom.DEFAULT_SIZE)
}

func (t *Tabs) Apply(node L.NodeType) (bool, error) {
	return false, nil
}

func (t *Tabs) Send(msg mux.Msg) {
}

func (t *Tabs) Resize(size geom.Size) error {
	return nil
}

func New(ctx context.Context) *Tabs {
	tabs := &Tabs{
		UpdatePublisher: mux.NewPublisher(),
	}

	return tabs
}
