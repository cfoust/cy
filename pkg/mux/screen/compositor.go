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
		go func() {
			updates := screen.Updates()
			for {
				select {
				case <-updates.Recv():
					c.Rerender()
				case <-depends.Ctx().Done():
					return
				}
			}
		}()
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
