package replay

import (
	"regexp"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/replay/movement/motion"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
)

type incrementalSearch struct {
	origin    geom.Vec2
	input     textinput.Model
	pattern   *regexp.Regexp
	isForward bool
	isActive  bool
}

func (i *incrementalSearch) Start(m motion.Movable, isForward bool) {
	i.origin = m.Cursor()
	i.isForward = isForward
	i.isActive = true
	i.input.Reset()
}

func createSafeRegex(expr string) (re *regexp.Regexp, err error) {
	re, err = regexp.Compile(expr)
	if err == nil {
		return
	}

	re, err = regexp.Compile(regexp.QuoteMeta(expr))
	return
}

func (i *incrementalSearch) getPattern() (re *regexp.Regexp, err error) {
	return createSafeRegex(i.input.Value())
}

func (i *incrementalSearch) Accept() {
	i.isActive = false
	if pattern, err := i.getPattern(); err == nil {
		i.pattern = pattern
	}
}

func (i *incrementalSearch) Cancel(m motion.Movable) {
	i.isActive = false
	m.Goto(i.origin)
}

func (i *incrementalSearch) IsActive() bool {
	return i.isActive
}

func (i *incrementalSearch) Next(m motion.Movable, isForward bool) {
	if i.pattern == nil {
		return
	}

	to, ok := motion.FindNext(
		m,
		i.pattern,
		m.Cursor(),
		isForward,
	)
	if !ok {
		return
	}

	m.Goto(to.Root())
}

func (i *incrementalSearch) Update(m motion.Movable, msg tea.Msg) {
	key, ok := msg.(taro.KeyMsg)
	if !ok {
		return
	}

	i.input, _ = i.input.Update(key.ToTea())
	pattern, err := i.getPattern()
	if err != nil {
		return
	}

	to, ok := motion.FindNext(
		m,
		pattern,
		i.origin,
		i.isForward,
	)
	if !ok {
		return
	}

	m.Goto(to.Root())
}

func newIncremental() *incrementalSearch {
	input := textinput.New()
	input.Focus()
	input.CharLimit = 0
	input.Width = 32
	input.Prompt = ""

	return &incrementalSearch{
		input: input,
	}
}
