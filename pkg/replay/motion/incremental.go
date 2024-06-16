package motion

import (
	"regexp"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
)

// Incremental is a state machine that implements vim/emacs style incremental
// searching for a Movable.
type Incremental struct {
	origin    geom.Vec2
	pattern   *regexp.Regexp
	isForward bool
	isActive  bool
	input     string
	result    emu.ScreenLine
}

// Start initializes the incremental search by saving the original cursor
// location and direction.
func (i *Incremental) Start(m Movable, isForward bool) {
	i.origin = m.Cursor()
	i.isForward = isForward
	i.isActive = true
	i.input = ""
}

func createSafeRegex(expr string) (re *regexp.Regexp, err error) {
	re, err = regexp.Compile(expr)
	if err == nil {
		return
	}

	re, err = regexp.Compile(regexp.QuoteMeta(expr))
	return
}

func (i *Incremental) getPattern() (re *regexp.Regexp, err error) {
	return createSafeRegex(i.input)
}

func (i *Incremental) Highlight() (line emu.ScreenLine, ok bool) {
	if !i.isActive {
		return
	}
	return i.result, true
}

// Accept confirms the motion and stores the pattern. This is equivalent to
// pressing enter when searching incrementally in vim.
func (i *Incremental) Accept() {
	i.isActive = false
	if pattern, err := i.getPattern(); err == nil {
		i.pattern = pattern
	}
}

// Cancel stops searching incrementally and returns to the origin.
func (i *Incremental) Cancel(m Movable) {
	i.isActive = false
	m.Goto(i.origin)
}

// IsActive returns true if the user is currently entering a search pattern.
func (i *Incremental) IsActive() bool {
	return i.isActive
}

func (i *Incremental) next(
	m Movable,
	pattern *regexp.Regexp,
	origin geom.Vec2,
	isForward bool,
	didLoop bool,
) {
	to, ok := FindNext(m, pattern, origin, isForward)
	if ok {
		m.Goto(to.Root())
		i.result = to
		return
	}

	if didLoop {
		return
	}

	// Loop around from beginning (or end)
	origin = geom.Vec2{
		R: 0,
		C: -1,
	}
	if !isForward {
		lastRow := m.NumLines() - 1
		lastLine, lineOk := m.Line(lastRow)
		if !lineOk {
			return
		}

		origin = geom.Vec2{
			R: lastRow,
			C: len(lastLine),
		}
	}

	i.next(
		m,
		pattern,
		origin,
		isForward,
		true,
	)
}

// Next jumps to the next instance of the accepted pattern in the direction of
// the search. `isForward` is relative to the direction of the original search;
// if the original search was backwards, `isForward=true` means that the search
// will go backwards.
func (i *Incremental) Next(m Movable, isForward bool) {
	if i.pattern == nil {
		return
	}

	i.next(
		m,
		i.pattern,
		m.Cursor(),
		// [Original] [Direction] = [Final]
		// F T = F
		// F F = T
		// T T = T
		// T F = F
		i.isForward == isForward,
		false,
	)
}

// Pattern takes a new pattern and jumps to the closest instance of it after
// the origin in the direction of the search.
func (i *Incremental) Pattern(m Movable, input string) {
	pattern, err := createSafeRegex(input)
	if err != nil {
		return
	}

	i.input = input
	i.next(m, pattern, i.origin, i.isForward, false)
}

func NewIncremental() *Incremental {
	return &Incremental{}
}
