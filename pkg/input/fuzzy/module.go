package fuzzy

import (
	"context"

	"github.com/cfoust/cy/pkg/anim"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/input/fuzzy/preview"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/rs/zerolog/log"
)

type Fuzzy struct {
	util.Lifetime
	anim *taro.Program

	// The initial state of the screen before fuzzy was started.
	initial image.Image

	result chan<- interface{}
	size   geom.Vec2

	render    *taro.Renderer
	textInput textinput.Model

	// Don't allow Fuzzy to quit or the user to choose anything
	isSticky bool

	// Whether Fuzzy should display options above or below the input.
	isUp bool

	// Whether to render at `location` instead of filling the boundaries of
	// the screen.
	isInline bool
	location geom.Vec2

	// before the user has done anything, we don't show the preview window
	haveMoved bool

	options  []Option
	filtered []Option
	selected int
	pattern  string

	// The number of options rendered on the last call View(). Used for
	// paging behavior.
	numRenderedOptions int

	// shown before the number of items
	prompt string

	// whether search should be case-sensitive
	caseSensitive bool

	// headers for the table
	headers []string

	tree            *tree.Tree
	server          *server.Server
	client          *server.Client
	preview         mux.Screen
	previewLifetime util.Lifetime
}

var _ taro.Model = (*Fuzzy)(nil)

func (f *Fuzzy) quit() (taro.Model, tea.Cmd) {
	if f.isSticky {
		return f, nil
	}
	return f, tea.Batch(
		func() tea.Msg {
			f.Cancel()
			return nil
		},
		tea.Quit,
	)
}

func (f *Fuzzy) Init() taro.Cmd {
	cmds := []taro.Cmd{
		textinput.Blink,
	}

	if f.anim != nil {
		w := taro.NewWatcher(f.Ctx(), f.anim)
		cmds = append(cmds, w.Wait())
	}

	return tea.Batch(cmds...)
}

func (f *Fuzzy) getOptions() []Option {
	if len(f.pattern) > 0 {
		return f.filtered
	}
	return f.options
}

type SelectedEvent struct {
	Option Option
}

func (f *Fuzzy) setSelected(index int) taro.Cmd {
	f.selected = geom.Max(
		0,
		geom.Clamp(index, 0, len(f.getOptions())-1),
	)

	f.preview = f.getPreview()

	var cmds []taro.Cmd
	if f.preview != nil {
		w := taro.NewWatcher(f.Ctx(), f.preview)
		cmds = append(
			cmds,
			w.Wait(),
		)
	}

	cmds = append(
		cmds,
		f.emitOption(),
	)

	return tea.Batch(cmds...)
}

func (f *Fuzzy) emitOption() taro.Cmd {
	if len(f.getOptions()) == 0 {
		return nil
	}

	return func() taro.Msg {
		log.Info().Msgf("emitting %+v", f.getOptions()[f.selected])
		return taro.PublishMsg{
			Msg: SelectedEvent{
				Option: f.getOptions()[f.selected],
			},
		}
	}
}

func (f *Fuzzy) getPreview() mux.Screen {
	options := f.getOptions()
	if len(options) == 0 {
		return nil
	}

	if f.preview != nil {
		f.previewLifetime.Cancel()
	}

	option := options[f.selected]
	if option.Preview == nil {
		return nil
	}

	f.previewLifetime = util.NewLifetime(f.Ctx())
	p := preview.New(
		f.previewLifetime.Ctx(),
		f.tree,
		f.client,
		f.server,
		f.initial.Clone(),
		option.Preview,
	)
	if p == nil {
		f.previewLifetime.Cancel()
		return nil
	}

	switch option.Preview.(type) {
	case preview.FrameType,
		preview.AnimationType,
		preview.LayoutType:
		p.Resize(f.size)
	default:
		p.Resize(geom.DEFAULT_SIZE)
	}

	return p
}

type Setting func(context.Context, *Fuzzy)

// WithInitial provides the initial background image. Used independently of
// WithAnimation to still support animation previews even if animation is
// turned off.
func WithInitial(initial image.Image) Setting {
	return func(ctx context.Context, f *Fuzzy) {
		f.initial = initial
	}
}

func WithAnimation(initial image.Image, creator anim.Creator) Setting {
	return func(ctx context.Context, f *Fuzzy) {
		f.initial = initial
		f.anim = anim.NewAnimator(
			ctx,
			creator(),
			initial,
			23,
		)
	}
}

// Don't allow Fuzzy to quit.
func WithSticky(ctx context.Context, f *Fuzzy) {
	f.isSticky = true
}

// If both of these are provided, Fuzzy can show previews for panes.
func WithNodes(
	tree *tree.Tree,
	server *server.Server,
) Setting {
	return func(ctx context.Context, f *Fuzzy) {
		f.tree = tree
		f.server = server
		f.client = server.AddClient(ctx, geom.Vec2{})
	}
}

func WithResult(result chan<- interface{}) Setting {
	return func(ctx context.Context, f *Fuzzy) {
		f.result = result
	}
}

// Displays Fuzzy as a small window at this location on the screen.
func WithInline(location, size geom.Vec2) Setting {
	return func(ctx context.Context, f *Fuzzy) {
		f.isInline = true
		f.location = location
		f.isUp = f.location.R > (size.R / 2)
	}
}

func WithReverse(ctx context.Context, f *Fuzzy) {
	f.isUp = false
}

func WithPrompt(prompt string) Setting {
	return func(ctx context.Context, f *Fuzzy) {
		f.prompt = prompt
	}
}

func WithHeaders(headers ...string) Setting {
	return func(ctx context.Context, f *Fuzzy) {
		f.headers = headers
	}
}

// WithCaseSensitive determines whether the matching algorithm will be
// case-sensitive.
func WithCaseSensitive(value bool) Setting {
	return func(ctx context.Context, f *Fuzzy) {
		f.caseSensitive = value
	}
}

func newFuzzy(
	ctx context.Context,
	options []Option,
	settings ...Setting,
) *Fuzzy {
	ti := textinput.New()
	ti.Focus()
	ti.Width = 20
	ti.Prompt = ""

	f := &Fuzzy{
		Lifetime:  util.NewLifetime(ctx),
		render:    taro.NewRenderer(),
		options:   options,
		selected:  0,
		textInput: ti,
		isUp:      true,
	}

	for _, setting := range settings {
		setting(f.Ctx(), f)
	}
	return f
}

func New(
	ctx context.Context,
	options []Option,
	settings ...Setting,
) *taro.Program {
	f := newFuzzy(ctx, options, settings...)
	return taro.New(f.Ctx(), f)
}

type matchResult struct {
	Filtered []Option
}

func queryOptions(
	options []Option,
	pattern string,
	caseSensitive bool,
) tea.Cmd {
	return func() tea.Msg {
		return matchResult{
			Filtered: Filter(
				options,
				pattern,
				caseSensitive,
			),
		}
	}
}
