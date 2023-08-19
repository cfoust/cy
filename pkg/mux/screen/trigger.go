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
	changes *mux.UpdatePublisher
	r       Renderable
	size    Size
}

func (t *Trigger) State() *tty.State {
	t.RLock()
	size := t.size
	t.RUnlock()
	return t.r.Render(size)
}

func (t *Trigger) Updates() *Updater {
	return t.changes.Subscribe()
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
	t.Rerender()
	return nil
}

func (t *Trigger) Rerender() {
	t.changes.Publish(t.State())
}

func NewTrigger(r Renderable) *Trigger {
	return &Trigger{
		changes: mux.NewPublisher(),
		r:       r,
	}
}
