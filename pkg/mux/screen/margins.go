package screen

import (
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

	bounds geom.Rect
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

func fitMargins(outer, margins Size) geom.Rect {
	rows := outer.R
	if margins.R > 0 {
		rows = outer.R - (margins.R * 2)
	}

	cols := outer.C
	if margins.C > 0 {
		cols = outer.C - (margins.C * 2)
	}

	return centerRect(outer, Size{
		R: geom.Max(rows, 1),
		C: geom.Max(cols, 1),
	})
}

func fitSize(outer, size Size) geom.Rect {
	rows := size.R
	if rows > outer.R {
		rows = outer.R
	}

	cols := size.C
	if cols > outer.C {
		cols = outer.C
	}

	return centerRect(outer, Size{
		R: geom.Max(rows, 1),
		C: geom.Max(cols, 1),
	})
}

func (l *Margins) State() *tty.State {
	l.RLock()
	defer l.RUnlock()

	bounds := l.bounds
	state := tty.New(bounds.Size())
	tty.Copy(bounds.Position(), state, l.screen.State())
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

func (l *Margins) Resize(size Size) error {
	inner := l.getInner(size)

	l.Lock()
	l.bounds = inner
	l.Unlock()

	err := l.screen.Resize(Size{
		R: inner.R,
		C: inner.C,
	})
	if err != nil {
		return err
	}

	l.rerender()
	return nil
}

func NewMargins(screen Screen) *Margins {
	return &Margins{
		changes: mux.NewPublisher(),
		size: Size{
			C: 80,
		},
		screen: screen,
	}
}
