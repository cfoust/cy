package screen

import (
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"

	"github.com/sasha-s/go-deadlock"
)

type Renderable interface {
	Render(Size) *tty.State
	mux.Resizable
}

type Trigger struct {
	deadlock.RWMutex
	*mux.UpdatePublisher
	r    Renderable
	size Size
}

func (t *Trigger) State() *tty.State {
	t.RLock()
	size := t.size
	t.RUnlock()
	return t.r.Render(size)
}

func (t *Trigger) Size() Size {
	t.RLock()
	defer t.RUnlock()
	return t.size
}

func (t *Trigger) Resize(size Size) error {
	t.Lock()
	t.size = size
	t.Unlock()
	t.Notify()
	return nil
}

func NewTrigger(r Renderable) *Trigger {
	return &Trigger{
		UpdatePublisher: mux.NewPublisher(),
		r:               r,
	}
}
