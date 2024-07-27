package layout

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/sasha-s/go-deadlock"
)

// Split renders two screens side by side (or one above the other) at a fixed proportion of its full width or height (respectively.)
type Split struct {
	deadlock.RWMutex
	*mux.UpdatePublisher

	screenA, screenB mux.Screen

	// The size of the Split's screen.
	size geom.Size

	// The location of screen B on the screen.
	positionB geom.Size

	// Whether the split should be vertical or horizontal.
	isVertical bool

	// Whether screenA or screenB "lead" to a screen that is attached. At
	// most one of these can be true at a time.
	isAttachedA, isAttachedB bool

	// Whether the number of cells was set directly.
	isCells bool

	// The proportion of the axis perpendicular to the split that A should
	// occupy.
	percent int // [0, 100]

	// The number of cells perpendicular to the split axis to include from
	// screen A. This is calculated using `percent`.
	cells int
}

var _ mux.Screen = (*Split)(nil)

func (s *Split) Kill() {
	s.RLock()
	var (
		screenA = s.screenA
		screenB = s.screenB
	)
	s.RUnlock()

	screenA.Kill()
	screenB.Kill()
}

func (s *Split) State() *tty.State {
	s.RLock()
	var (
		size        = s.size
		positionB   = s.positionB
		isAttachedA = s.isAttachedA
		isAttachedB = s.isAttachedB
	)
	s.RUnlock()

	state := tty.New(size)

	stateA := s.screenA.State().Clone()
	if !isAttachedA && stateA.CursorVisible {
		cursor := stateA.Cursor
		stateA.Image[cursor.R][cursor.C].BG = 8
	}
	image.CopyRaw(geom.Size{}, state.Image, stateA.Image)

	stateB := s.screenB.State().Clone()
	if !isAttachedB && stateB.CursorVisible {
		cursor := stateB.Cursor
		stateB.Image[cursor.R][cursor.C].BG = 8
	}
	image.CopyRaw(positionB, state.Image, stateB.Image)

	if isAttachedA {
		state.Cursor = stateA.Cursor
		state.CursorVisible = stateA.CursorVisible
	} else if isAttachedB {
		cursor := stateB.Cursor
		cursor.C += positionB.C
		cursor.R += positionB.R
		state.Cursor = cursor
		state.CursorVisible = stateB.CursorVisible
	} else {
		state.CursorVisible = false
	}

	return state
}

func (s *Split) SetAttached(isAttachedA, isAttachedB bool) {
	s.Lock()
	defer s.Unlock()
	s.isAttachedA = isAttachedA
	s.isAttachedB = isAttachedB
}

func (s *Split) Send(msg mux.Msg) {
	if !s.isAttachedA && !s.isAttachedB {
		return
	}

	// We don't need to translate the cursor position if screen is on the
	// left or the top
	if s.isAttachedA {
		s.screenA.Send(msg)
		return
	}

	s.RLock()
	positionB := s.positionB
	s.RUnlock()

	s.screenB.Send(taro.TranslateMouseMessage(
		msg,
		-positionB.C,
		-positionB.R,
	))
}

func (s *Split) poll(ctx context.Context) {
	updatesA := s.screenA.Subscribe(ctx)
	updatesB := s.screenB.Subscribe(ctx)

	for {
		select {
		case <-ctx.Done():
			return
		case event := <-updatesA.Recv():
			s.Publish(event)
		case event := <-updatesB.Recv():
			s.Publish(event)
		}
	}
}

func (s *Split) recalculate() error {
	size := s.size

	axisCells := size.C
	if s.isVertical {
		axisCells = size.R
	}

	desiredCells := s.cells
	if !s.isCells {
		proportion := float64(s.percent) / 100.
		desiredCells = int((proportion * float64(axisCells)))
	}

	desiredCells = geom.Clamp(
		desiredCells,
		1,
		axisCells-1,
	)

	positionB := geom.Size{C: desiredCells}
	if s.isVertical {
		positionB = geom.Size{R: desiredCells}
	}
	s.positionB = positionB

	sizeA := geom.Size{
		R: size.R,
		C: desiredCells,
	}
	if s.isVertical {
		sizeA = geom.Size{
			R: desiredCells,
			C: size.C,
		}
	}
	err := s.screenA.Resize(sizeA)
	if err != nil {
		return err
	}

	sizeB := size.Sub(positionB)
	err = s.screenB.Resize(sizeB)
	if err != nil {
		return err
	}

	return nil
}

func (s *Split) setPercent(percent int) {
	s.percent = geom.Clamp(percent, 0, 100)
	s.isCells = false
}

func (s *Split) SetPercent(percent int) error {
	s.Lock()
	defer s.Unlock()
	s.setPercent(percent)
	return s.recalculate()
}

func (s *Split) setCells(cells int) {
	s.cells = cells
	s.isCells = true
}

func (s *Split) Resize(size geom.Size) error {
	s.Lock()
	defer s.Unlock()
	s.size = size
	return s.recalculate()
}

func NewSplit(
	ctx context.Context,
	screenA, screenB mux.Screen,
	isVertical bool,
) *Split {
	split := &Split{
		UpdatePublisher: mux.NewPublisher(),
		screenA:         screenA,
		screenB:         screenB,
		isVertical:      isVertical,
		percent:         50,
	}

	go split.poll(ctx)

	return split
}
