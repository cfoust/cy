package fuzzy

import (
	"context"

	"github.com/cfoust/cy/pkg/anim"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
)

type Fuzzy struct {
	util.Lifetime
	anim *anim.Animator

	result   chan<- interface{}
	location geom.Vec2
	size     geom.Vec2

	render    *taro.Renderer
	textInput textinput.Model

	// before the user has done anything, we don't show the preview window
	haveMoved bool

	options  []Option
	filtered []Option
	selected int
	pattern  string

	tree       *tree.Tree
	client     *server.Client
	isAttached bool
}

var _ taro.Model = (*Fuzzy)(nil)

func (f *Fuzzy) quit() (taro.Model, tea.Cmd) {
	return f, tea.Quit
}

type AttachEvent struct {
	pane *tree.Pane
}

type DetachEvent struct {
}

func (f *Fuzzy) Attach(id tree.NodeID) taro.Cmd {
	f.isAttached = false

	return func() tea.Msg {
		pane, ok := f.tree.PaneById(id)
		if !ok {
			return nil
		}

		f.client.Attach(f.Ctx(), pane.Screen())
		return AttachEvent{
			pane: pane,
		}
	}
}

func (f *Fuzzy) handlePreview() taro.Cmd {
	options := f.getOptions()
	if len(options) == 0 {
		return nil
	}

	option := options[f.selected]
	if option.Preview == nil {
		return nil
	}

	switch preview := option.Preview.(type) {
	case tree.NodeID:
		return f.Attach(preview)
	}

	return nil
}

func (f *Fuzzy) Init() taro.Cmd {
	return tea.Batch(
		textinput.Blink,
		taro.WaitScreens(f.Ctx(), f.anim),
	)
}

func (f *Fuzzy) getOptions() []Option {
	if len(f.pattern) > 0 {
		return f.filtered
	}
	return f.options
}

func (f *Fuzzy) isInverted() bool {
	return f.location.R > (f.size.R / 2)
}

func (f *Fuzzy) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	var cmds []tea.Cmd
	var cmd tea.Cmd

	switch msg := msg.(type) {
	case AttachEvent:
		f.isAttached = true
		return f, func() tea.Msg {
			select {
			case <-f.client.Attachment().Ctx().Done():
				return nil
			case <-msg.pane.Ctx().Done():
				return DetachEvent{}
			}
		}
	case DetachEvent:
		f.isAttached = false
		return f, nil
	case taro.ScreenUpdate:
		return f, taro.WaitScreens(f.Ctx(), f.anim)
	case matchResult:
		f.filtered = msg.Filtered
		f.selected = geom.Max(geom.Min(f.selected, len(f.getOptions())-1), 0)
		return f, nil
	case tea.WindowSizeMsg:
		size := geom.Size{
			R: msg.Height,
			C: msg.Width,
		}
		f.anim.Resize(size)
		f.size = size
	case taro.KeyMsg:
		switch msg.Type {
		case taro.KeyEsc, taro.KeyCtrlC:
			f.result <- nil
			return f.quit()
		case taro.KeyUp, taro.KeyCtrlK:
			f.haveMoved = true
			delta := -1
			if f.isInverted() {
				delta = 1
			}
			f.selected = geom.Clamp(f.selected+delta, 0, len(f.getOptions()))
			return f, f.handlePreview()
		case taro.KeyDown, taro.KeyCtrlJ:
			f.haveMoved = true
			delta := 1
			if f.isInverted() {
				delta = -1
			}
			f.selected = geom.Clamp(f.selected+delta, 0, len(f.getOptions())-1)
			return f, f.handlePreview()
		case taro.KeyEnter:
			if f.selected < len(f.getOptions()) {
				option := f.getOptions()[f.selected]
				f.result <- option.Result
			} else {
				f.result <- nil
			}
			return f.quit()
		}
	}

	inputMsg := msg
	// We need to translate taro.KeyMsg to tea.KeyMsg (for now)
	if key, ok := msg.(taro.KeyMsg); ok {
		inputMsg = key.ToTea()
	}
	f.textInput, cmd = f.textInput.Update(inputMsg)
	cmds = append(cmds, cmd)

	value := f.textInput.Value()
	if f.pattern != value {
		f.pattern = value
		cmds = append(cmds, queryOptions(f.options, value))
	}

	return f, tea.Batch(cmds...)
}

func NewFuzzy(
	ctx context.Context,
	bg image.Image,
	options []Option,
	location geom.Vec2,
	tree *tree.Tree,
	client *server.Client,
	result chan<- interface{},
) *taro.Program {
	ti := textinput.New()
	ti.Focus()
	ti.CharLimit = 20
	ti.Width = 20
	ti.Prompt = ""

	return taro.New(ctx, &Fuzzy{
		Lifetime: util.NewLifetime(ctx),
		anim: anim.NewAnimator(
			ctx,
			anim.Random(),
			bg,
			23,
		),
		render:    taro.NewRenderer(),
		result:    result,
		location:  location,
		options:   options,
		selected:  0,
		textInput: ti,
		tree:      tree,
		client:    client,
	})
}

type matchResult struct {
	Filtered []Option
}

func queryOptions(options []Option, pattern string) tea.Cmd {
	return func() tea.Msg {
		return matchResult{
			Filtered: Filter(options, pattern),
		}
	}
}
