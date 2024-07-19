package screen

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/sasha-s/go-deadlock"
)

// Split renders two screens side by side (or one above the other) at a fixed proportion of its full width or height (respectively.)
type Split struct {
	deadlock.RWMutex
	*mux.UpdatePublisher

	screenA, screenB Screen

	// The size of the Split's screen.
	size Size
	// Whether the split should be vertical or horizontal.
	isVertical bool
	// Whether screenA is focused, which means its cursor position will be
	// used and causes events to be sent to it.
	isFocusA bool

	// The proportion of the axis perpendicular to the split that A should occupy.
	percent float64 // [0..1]

	// The number of cells perpendicular to the split axis to include from
	// screen A. This is calculated using `percent`.
	splitCells int
}

var _ Screen = (*Split)(nil)

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

func (s *Split) getPositionB() Size {
	positionB := geom.Size{C: s.splitCells}
	if s.isVertical {
		positionB = geom.Size{R: s.splitCells}
	}

	return positionB
}

func (s *Split) State() *tty.State {
	s.RLock()
	var (
		size      = s.size
		positionB = s.getPositionB()
	)
	s.RUnlock()

	state := tty.New(size)
	stateA := s.screenA.State()
	tty.Copy(geom.Size{}, state, stateA)
	stateB := s.screenB.State()
	tty.Copy(positionB, state, stateB)

	return state
}

func (s *Split) Send(msg mux.Msg) {
	if s.isFocusA {
		s.screenA.Send(msg)
		return
	}

	s.RLock()
	positionB := s.getPositionB()
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

func (s *Split) recalculate(size Size) error {
	s.size = size

	axisCells := size.C
	if s.isVertical {
		axisCells = size.R
	}

	s.splitCells = geom.Max(int(s.percent*float64(axisCells)), 1)
	positionB := s.getPositionB()
	sizeA := geom.Size{
		R: size.R,
		C: s.splitCells,
	}
	if s.isVertical {
		sizeA = geom.Size{
			R: s.splitCells,
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

func (s *Split) SetPercent(percent float64) error {
	s.Lock()
	defer s.Unlock()
	s.percent = percent
	return s.recalculate(s.size)
}

func (s *Split) Resize(size Size) error {
	s.Lock()
	defer s.Unlock()

	return s.recalculate(size)
}

func NewSplit(
	ctx context.Context,
	screenA, screenB Screen,
	percent float64,
	isVertical bool,
) *Split {
	split := &Split{
		UpdatePublisher: mux.NewPublisher(),
		screenA:         screenA,
		screenB:         screenB,
		isVertical:      isVertical,
		percent:         percent,
	}

	go split.poll(ctx)

	return split
}
