package thumbs

import (
	"context"
	"regexp"
	"strings"

	"github.com/cfoust/cy/pkg/anim"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/params"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
)

// Match represents a text match found on the screen
type Match struct {
	Text    string
	Pattern string
	X, Y    int
	Hint    string
}

// Thumbs represents the thumbs picker interface
type Thumbs struct {
	util.Lifetime
	anim   *taro.Program
	params *params.Parameters

	// The initial state of the screen before thumbs was started
	initial image.Image
	result  chan<- interface{}
	size    geom.Vec2

	render    *taro.Renderer
	textInput textinput.Model

	// Whether to render at `location` instead of filling the boundaries of
	// the screen
	isInline bool
	location geom.Vec2

	matches  []Match
	selected int

	// The alphabet to use for generating hints
	alphabet []rune
}

var _ taro.Model = (*Thumbs)(nil)

// Default regex patterns from tmux-thumbs
var defaultPatterns = []struct {
	name    string
	pattern string
}{
	{"markdown_url", `\[[^\]]*\]\(([^)]+)\)`},
	{"url", `(?P<match>(https?://|git@|git://|ssh://|ftp://|file:///)[^\s]+)`},
	{"diff_summary", `diff --git a/([.\w\-@~\[\]]+?/[.\w\-@\[\]]++) b/([.\w\-@~\[\]]+?/[.\w\-@\[\]]++)`},
	{"diff_a", `--- a/([^\s]+)`},
	{"diff_b", `\+\+\+ b/([^\s]+)`},
	{"docker", `sha256:([0-9a-f]{64})`},
	{"path", `(?P<match>([.\w\-@$~\[\]]+)?(/[.\w\-@$\[\]]+)+)`},
	{"color", `#[0-9a-fA-F]{6}`},
	{"uid", `[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}`},
	{"ipfs", `Qm[0-9a-zA-Z]{44}`},
	{"sha", `[0-9a-f]{7,40}`},
	{"ip", `\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}`},
	{"ipv6", `[A-f0-9:]+:+[A-f0-9:]+[%\w\d]+`},
	{"address", `0x[0-9a-fA-F]+`},
	{"number", `[0-9]{4,}`},
}

// Default alphabet for generating hints (qwerty layout)
var defaultAlphabet = []rune("asdfqwerzxcvjklmiuopghtybn")

func (t *Thumbs) quit() (taro.Model, tea.Cmd) {
	return t, tea.Batch(
		func() tea.Msg {
			t.Cancel()
			return nil
		},
		tea.Quit,
	)
}

func (t *Thumbs) Init() taro.Cmd {
	cmds := []taro.Cmd{
		textinput.Blink,
	}

	if t.anim != nil {
		w := taro.NewWatcher(t.Ctx(), t.anim)
		cmds = append(cmds, w.Wait())
	}

	return tea.Batch(cmds...)
}

// generateHints creates hint strings for the given number of matches
func (t *Thumbs) generateHints(numMatches int) []string {
	hints := make([]string, 0, numMatches)
	alphabet := t.alphabet
	alphabetLen := len(alphabet)

	for i := 0; i < numMatches; i++ {
		hint := ""
		num := i
		for {
			hint = string(alphabet[num%alphabetLen]) + hint
			num = num / alphabetLen
			if num == 0 {
				break
			}
			num-- // Adjust for 1-based indexing
		}
		hints = append(hints, hint)
	}

	return hints
}

// findMatches searches for all pattern matches in the screen content
func (t *Thumbs) findMatches(lines []string, customPatterns []string) []Match {
	var matches []Match

	// Compile all patterns
	var patterns []*regexp.Regexp
	patternNames := make([]string, 0)

	// Add custom patterns first (higher priority)
	for _, pattern := range customPatterns {
		if compiled, err := regexp.Compile(pattern); err == nil {
			patterns = append(patterns, compiled)
			patternNames = append(patternNames, "custom")
		}
	}

	// Add default patterns
	for _, p := range defaultPatterns {
		if compiled, err := regexp.Compile(p.pattern); err == nil {
			patterns = append(patterns, compiled)
			patternNames = append(patternNames, p.name)
		}
	}

	// Search through each line
	for y, line := range lines {
		for i, pattern := range patterns {
			patternName := patternNames[i]
			allMatches := pattern.FindAllStringSubmatch(line, -1)
			allIndices := pattern.FindAllStringSubmatchIndex(line, -1)

			for j, match := range allMatches {
				if len(match) == 0 {
					continue
				}

				indices := allIndices[j]

				// Determine the actual match text
				var matchText string
				var startX int

				if len(match) > 1 {
					// Use first capture group if available
					matchText = match[1]
					// Find the position of the capture group
					startX = strings.Index(line[indices[0]:], matchText) + indices[0]
				} else {
					// Use entire match
					matchText = match[0]
					startX = indices[0]
				}

				if strings.TrimSpace(matchText) == "" {
					continue
				}

				matches = append(matches, Match{
					Text:    matchText,
					Pattern: patternName,
					X:       startX,
					Y:       y,
				})
			}
		}
	}

	// Generate hints for matches
	hints := t.generateHints(len(matches))
	for i := range matches {
		if i < len(hints) {
			matches[i].Hint = hints[i]
		}
	}

	return matches
}

type Setting func(context.Context, *Thumbs)

// WithInitial provides the initial background image
func WithInitial(initial image.Image) Setting {
	return func(ctx context.Context, t *Thumbs) {
		t.initial = initial
	}
}

// WithAnimation provides a specific animation to render in the background
func WithAnimation(initial image.Image, creator anim.Creator) Setting {
	return func(ctx context.Context, t *Thumbs) {
		t.initial = initial
		t.anim = anim.NewAnimator(
			ctx,
			creator(),
			initial,
			23,
		)
	}
}

func WithResult(result chan<- interface{}) Setting {
	return func(ctx context.Context, t *Thumbs) {
		t.result = result
	}
}

// WithInline displays Thumbs as a small window at this location on the screen
func WithInline(location, size geom.Vec2) Setting {
	return func(ctx context.Context, t *Thumbs) {
		t.isInline = true
		t.location = location
	}
}

// WithParams provides parameters this Thumbs will use for rendering
func WithParams(params *params.Parameters) Setting {
	return func(ctx context.Context, t *Thumbs) {
		t.params = params
	}
}

// WithAlphabet sets the alphabet to use for generating hints
func WithAlphabet(alphabet string) Setting {
	return func(ctx context.Context, t *Thumbs) {
		if alphabet != "" {
			t.alphabet = []rune(alphabet)
		}
	}
}

func newThumbs(
	ctx context.Context,
	screenLines []string,
	customPatterns []string,
	settings ...Setting,
) *Thumbs {
	ti := textinput.New()
	ti.Focus()
	ti.Width = 20
	ti.Prompt = ""

	t := &Thumbs{
		Lifetime:  util.NewLifetime(ctx),
		render:    taro.NewRenderer(),
		selected:  0,
		textInput: ti,
		alphabet:  defaultAlphabet,
		params:    params.New(),
	}

	for _, setting := range settings {
		setting(t.Ctx(), t)
	}

	// Find matches in the screen content
	t.matches = t.findMatches(screenLines, customPatterns)

	return t
}

func New(
	ctx context.Context,
	screenLines []string,
	customPatterns []string,
	settings ...Setting,
) *taro.Program {
	t := newThumbs(ctx, screenLines, customPatterns, settings...)
	return taro.New(t.Ctx(), t)
}

type SelectedEvent struct {
	Match Match
}

