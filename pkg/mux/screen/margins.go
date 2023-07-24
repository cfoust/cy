package screen

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"

	"github.com/sasha-s/go-deadlock"
)

// Margins puts empty space around a Screen and centers it. In size mode,
// Margins attempts to keep the Screen at a fixed size (one or both dimensions
// fixed). In margin mode, the Screen is surrounded by fixed-size margins that
// do not change with the screen size.
type Margins struct {
	deadlock.RWMutex
	changes *mux.UpdatePublisher
	screen  Screen

	// Whether Margins is in margin mode or size mode
	isMargins bool
	margins   Size
	size      Size

	outer Size
	inner geom.Rect
}

var _ Screen = (*Margins)(nil)

func getCenter(outer, inner int) int {
	offset := 0
	extra := outer - inner
	if extra > 0 {
		offset = extra / 2
	}

	return offset
}

func centerRect(outer, inner Size) geom.Rect {
	return geom.Rect{
		R: getCenter(outer.R, inner.R),
		C: getCenter(outer.C, inner.C),
		H: inner.R,
		W: inner.C,
	}
}

func fitMargin(outer, margin int) int {
	if margin == 0 {
		return outer
	}

	return geom.Max(outer-(margin*2), 1)
}

func fitMargins(outer, margins Size) geom.Rect {
	return centerRect(outer, Size{
		R: fitMargin(outer.R, margins.R),
		C: fitMargin(outer.C, margins.C),
	})
}

func getSize(outer, desired int) int {
	if desired == 0 {
		return outer
	}

	if desired > outer {
		return outer
	}

	return desired
}

func fitSize(outer, size Size) geom.Rect {
	return centerRect(outer, Size{
		R: geom.Max(getSize(outer.R, size.R), 1),
		C: geom.Max(getSize(outer.C, size.C), 1),
	})
}

func (l *Margins) State() *tty.State {
	l.RLock()
	inner := l.inner
	outer := l.outer
	l.RUnlock()

	state := tty.New(outer)
	tty.Copy(inner.Position(), state, l.screen.State())
	return state
}

func (l *Margins) Updates() *Updater {
	return l.changes.Subscribe()
}

func (l *Margins) rerender() {
	l.changes.Publish(l.State())
}

func (l *Margins) Write(data []byte) (n int, err error) {
	return l.screen.Write(data)
}

func (l *Margins) getInner(size Size) geom.Rect {
	l.RLock()
	defer l.RUnlock()

	if l.isMargins {
		return fitMargins(size, l.margins)
	}

	return fitSize(size, l.size)
}

func (l *Margins) poll(ctx context.Context) {
	updates := l.screen.Updates()
	for {
		select {
		case <-ctx.Done():
			return
		case <-updates.Recv():
			l.rerender()
		}
	}
}

func (l *Margins) Resize(size Size) error {
	inner := l.getInner(size)

	l.Lock()
	l.inner = inner
	l.outer = size
	l.Unlock()

	err := l.screen.Resize(inner.Size())
	if err != nil {
		return err
	}

	l.rerender()
	return nil
}

func NewMargins(ctx context.Context, screen Screen) *Margins {
	margins := &Margins{
		changes: mux.NewPublisher(),
		size: Size{
			C: 80,
		},
		screen: screen,
	}

	go margins.poll(ctx)

	return margins
}
