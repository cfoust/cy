package api

import (
	"github.com/cfoust/cy/pkg/emu"
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

type ScreenParams struct {
	Scrollback bool
}

func (p *PaneModule) Screen(
	id *janet.Value,
	params *janet.Named[ScreenParams],
) (ScreenResult, error) {
	defer id.Free()

	pane, err := resolvePane(p.Tree, id)
	if err != nil {
		return ScreenResult{}, err
	}

	values := params.Values()

	r, ok := pane.Screen().(*replayable.Replayable)
	if !ok {
		// Non-replayable pane: just return the tty state
		state := pane.Screen().State()
		lines := make([]string, 0, len(state.Image))
		for _, line := range state.Image {
			lines = append(lines, line.String())
		}
		return ScreenResult{
			Lines: lines,
		}, nil
	}

	term := r.Terminal()
	isAlt := term.IsAltMode()

	if values.Scrollback {
		return ScreenResult{
			Lines: linesToStrings(term.FlowLines()),
			IsAlt: isAlt,
		}, nil
	}

	return ScreenResult{
		Lines: linesToStrings(term.ScreenLines()),
		IsAlt: isAlt,
	}, nil
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
