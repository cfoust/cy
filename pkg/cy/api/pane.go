package api

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/keys"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/replay/replayable"
	"github.com/cfoust/cy/pkg/taro"
)

type PaneModule struct {
	Tree *tree.Tree
}

func (p *PaneModule) Attach(context interface{}, id *janet.Value) error {
	defer id.Free()

	client, err := getClient(context)
	if err != nil {
		return err
	}

	pane, err := resolvePane(p.Tree, id)
	if err != nil {
		return err
	}

	return client.Attach(pane)
}

func (p *PaneModule) HistoryForward(context interface{}) error {
	client, err := getClient(context)
	if err != nil {
		return err
	}

	return client.HistoryForward()
}

func (p *PaneModule) HistoryBackward(context interface{}) error {
	client, err := getClient(context)
	if err != nil {
		return err
	}

	return client.HistoryBackward()
}

func (p *PaneModule) Current(context interface{}) *tree.NodeID {
	// If exec context has a source node, return that
	if sourceNode := getSourceNode(context); sourceNode != nil {
		return sourceNode
	}

	// Fall back to client's currently viewed node
	client, ok := context.(Client)
	if !ok {
		return nil
	}

	node := client.Node()
	if node == nil {
		return nil
	}

	id := node.Id()
	return &id
}

type ScreenResult struct {
	Lines []string `janet:"lines"`
	IsAlt bool     `janet:"is-alt"`
}

func linesToStrings(lines []emu.Line) []string {
	strs := make([]string, len(lines))
	for i, line := range lines {
		strs[i] = line.String()
	}
	return strs
}

// imageToStrings renders the visible grid of a non-replayable pane,
// which is the only state such panes expose.
func imageToStrings(state *tty.State) []string {
	lines := make([]string, 0, len(state.Image))
	for _, line := range state.Image {
		lines = append(lines, line.String())
	}
	return lines
}

// Screen returns the visible grid of the pane along with whether the
// pane is currently in alternate screen mode (e.g. running vim or
// htop). This is a snapshot of what is displayed right now; for the
// full logical scrollback, use Scrollback.
func (p *PaneModule) Screen(id *janet.Value) (ScreenResult, error) {
	defer id.Free()

	pane, err := resolvePane(p.Tree, id)
	if err != nil {
		return ScreenResult{}, err
	}

	r, ok := pane.Screen().(*replayable.Replayable)
	if !ok {
		// Non-replayable pane: just return the tty state.
		return ScreenResult{
			Lines: imageToStrings(pane.Screen().State()),
		}, nil
	}

	term := r.Terminal()
	return ScreenResult{
		Lines: linesToStrings(term.ScreenLines()),
		IsAlt: term.IsAltMode(),
	}, nil
}

// Scrollback returns the full logical history of the pane (scrollback
// plus the current screen) as a flat array of lines, reflowed at the
// pane's current width. Unlike Screen, blank padding rows are
// collapsed and there is no alternate-screen indicator: this is the
// line-reading counterpart to Screen's grid snapshot.
func (p *PaneModule) Scrollback(id *janet.Value) ([]string, error) {
	defer id.Free()

	pane, err := resolvePane(p.Tree, id)
	if err != nil {
		return nil, err
	}

	r, ok := pane.Screen().(*replayable.Replayable)
	if !ok {
		// Non-replayable pane has no history; fall back to the
		// visible grid.
		return imageToStrings(pane.Screen().State()), nil
	}

	return linesToStrings(r.Terminal().FlowLines()), nil
}

func (p *PaneModule) SendKeys(id *janet.Value, keys []string) error {
	defer id.Free()

	pane, err := resolvePane(p.Tree, id)
	if err != nil {
		return err
	}

	for _, key := range taro.KeysToMsg(keys...) {
		pane.Screen().Send(key)
	}

	return nil
}

func (p *PaneModule) SendText(id *janet.Value, text string) error {
	defer id.Free()

	pane, err := resolvePane(p.Tree, id)
	if err != nil {
		return err
	}

	pane.Screen().Send(taro.KittyKeyMsg{
		Code: keys.KeyText,
		Text: text,
	})

	return nil
}

func (p *PaneModule) SendBytes(id *janet.Value, data string) error {
	defer id.Free()

	pane, err := resolvePane(p.Tree, id)
	if err != nil {
		return err
	}

	pane.Screen().Send(taro.RawMsg{
		Data: []byte(data),
	})

	return nil
}
