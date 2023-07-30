package screen

import (
	"context"

	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/util"

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

// A Compositor simplifies rendering a screen.
type Compositor struct {
	deadlock.RWMutex
	changes *mux.UpdatePublisher
	size    Size
	r       Renderable
	depends *util.Lifetime
}

func (c *Compositor) State() *tty.State {
	c.RLock()
	size := c.size
	c.RUnlock()
	return c.r.Render(size)
}

func (c *Compositor) Updates() *Updater {
	return c.changes.Subscribe()
}

func (c *Compositor) Rerender() {
	c.changes.Publish(c.State())
}

func (c *Compositor) Depends(ctx context.Context, screens ...mux.Screen) {
	c.Lock()
	defer c.Unlock()

	if c.depends != nil {
		c.depends.Cancel()
	}

	depends := util.NewLifetime(ctx)

	for _, screen := range screens {
		go func(screen Screen) {
			updates := screen.Updates()
			for {
				select {
				case <-updates.Recv():
					c.Rerender()
				case <-depends.Ctx().Done():
					return
				}
			}
		}(screen)
	}

	c.depends = &depends
}

func (c *Compositor) Resize(size Size) error {
	c.Lock()
	c.size = size
	c.Unlock()
	c.Rerender()
	return nil
}

func NewCompositor(ctx context.Context, r Renderable, screens ...Screen) *Compositor {
	c := &Compositor{
		changes: mux.NewPublisher(),
	}
	c.Depends(ctx, screens...)
	return c
}
