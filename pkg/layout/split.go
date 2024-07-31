package layout

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/charmbracelet/lipgloss"
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

	// Whether the number of cells was set directly.
	isCells bool

	// The proportion of the axis perpendicular to the split that A should
	// occupy.
	percent int // [0, 100]

	// The number of cells perpendicular to the split axis to include from
	// screen A. This is calculated using `percent`.
	cells int

	borderStyle *lipgloss.Border
}

var _ mux.Screen = (*Split)(nil)
var _ reusable = (*Split)(nil)

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
		isVertical  = s.isVertical
		borderStyle = s.borderStyle
	)
	s.RUnlock()

	state := tty.New(size)

	stateA := s.screenA.State().Clone()
	image.CopyRaw(geom.Size{}, state.Image, stateA.Image)

	if borderStyle != nil {
		if !isVertical {
			col := geom.Clamp(positionB.C-1, 0, size.C-1)
			char := []rune(borderStyle.Left)[0]
			for row := 0; row < size.R; row++ {
				state.Image[row][col].Char = char
			}
		} else {
			row := geom.Clamp(positionB.R-1, 0, size.R-1)
			char := []rune(borderStyle.Top)[0]
			for col := 0; col < size.C; col++ {
				state.Image[row][col].Char = char
			}
		}
	}

	stateB := s.screenB.State().Clone()
	image.CopyRaw(positionB, state.Image, stateB.Image)

	if stateA.CursorVisible {
		state.Cursor = stateA.Cursor
		state.CursorVisible = stateA.CursorVisible
	} else if stateB.CursorVisible {
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

func (s *Split) reuse(node NodeType) (bool, error) {
	config, ok := node.(SplitType)
	if !ok {
		return false, nil
	}

	var changed bool
	if config.Percent != nil && (s.isCells || s.percent != *config.Percent) {
		s.setPercent(*config.Percent)
		changed = true
	}

	if config.Cells != nil && (!s.isCells || s.cells != *config.Cells) {
		s.setCells(*config.Cells)
		changed = true
	}

	if config.Border != nil {
		s.borderStyle = &config.Border.Style
		changed = true
	}

	if !changed {
		return true, nil
	}

	return true, s.recalculate()
}

func (s *Split) Send(msg mux.Msg) {
	s.screenA.Send(msg)

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
			if _, ok := event.(nodeChangeEvent); ok {
				continue
			}
			s.Publish(event)
		case event := <-updatesB.Recv():
			if _, ok := event.(nodeChangeEvent); ok {
				continue
			}
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

	positionB := geom.Size{C: desiredCells}
	if s.isVertical {
		positionB = geom.Size{R: desiredCells}
	}

	s.positionB = positionB

	if s.borderStyle != nil {
		desiredCells--
	}

	desiredCells = geom.Clamp(
		desiredCells,
		1,
		axisCells-1,
	)

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

func (l *LayoutEngine) createSplit(
	node *screenNode,
	config SplitType,
) error {
	nodeA, err := l.createNode(node.Ctx(), config.A)
	if err != nil {
		return err
	}
	nodeB, err := l.createNode(node.Ctx(), config.B)
	if err != nil {
		return err
	}

	split := NewSplit(
		node.Ctx(),
		nodeA.Screen,
		nodeB.Screen,
		config.Vertical,
	)

	if config.Percent != nil {
		split.setPercent(*config.Percent)
	}

	if config.Cells != nil {
		split.setCells(*config.Cells)
	}

	if config.Border != nil {
		split.borderStyle = &config.Border.Style
	}

	node.Screen = split
	node.Children = []*screenNode{nodeA, nodeB}
	return nil
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
