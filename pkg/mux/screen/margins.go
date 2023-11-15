package screen

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/sasha-s/go-deadlock"
)

// Margins puts empty space around a Screen and centers it. In size mode,
// Margins attempts to keep the Screen at a fixed size (one or both dimensions
// fixed). In margin mode, the Screen is surrounded by fixed-size margins that
// do not change with the screen size.
type Margins struct {
	deadlock.RWMutex
	*mux.UpdatePublisher

	screen Screen

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
		Position: geom.Vec2{
			R: getCenter(outer.R, inner.R),
			C: getCenter(outer.C, inner.C),
		},
		Size: geom.Vec2{
			R: inner.R,
			C: inner.C,
		},
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

	innerState := l.screen.State()
	state := tty.New(outer)

	tty.Copy(inner.Position, state, innerState)

	size := state.Image.Size()
	for row := 0; row < size.R; row++ {
		for col := 0; col < size.C; col++ {
			if inner.Contains(geom.Vec2{
				R: row,
				C: col,
			}) {
				continue
			}
			state.Image[row][col].Transparent = true
		}
	}

	return state
}

func (l *Margins) rerender() {
}

func (l *Margins) Send(msg mux.Msg) {
	l.RLock()
	inner := l.inner
	l.RUnlock()
	l.screen.Send(taro.TranslateMouseMessage(
		msg,
		-inner.Position.C,
		-inner.Position.R,
	))
}

func (l *Margins) getInner(size Size) geom.Rect {
	if l.isMargins {
		return fitMargins(size, l.margins)
	}

	return fitSize(size, l.size)
}

func (l *Margins) SetSize(size Size) error {
	l.Lock()
	l.isMargins = false
	l.size = Size{
		R: geom.Max(0, size.R),
		C: geom.Max(0, size.C),
	}
	l.Unlock()
	return l.recalculate()
}

func (l *Margins) Size() Size {
	l.RLock()
	defer l.RUnlock()
	return l.size
}

func (l *Margins) poll(ctx context.Context) {
	updates := l.screen.Subscribe(ctx)

	for {
		select {
		case <-ctx.Done():
			return
		case event := <-updates.Recv():
			l.Publish(event)
		}
	}
}

func (l *Margins) recalculate() error {
	l.RLock()
	outer := l.outer
	l.RUnlock()

	inner := l.getInner(outer)

	l.Lock()
	l.inner = inner
	l.Unlock()

	err := l.screen.Resize(inner.Size)
	if err != nil {
		return err
	}

	l.rerender()
	return nil
}

func (l *Margins) Resize(size Size) error {
	l.Lock()
	l.outer = size
	l.Unlock()
	return l.recalculate()
}

func NewMargins(ctx context.Context, screen Screen) *Margins {
	margins := &Margins{
		UpdatePublisher: mux.NewPublisher(),
		size: Size{
			C: 80,
		},
		screen: screen,
	}

	go margins.poll(ctx)

	return margins
}
