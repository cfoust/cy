package api

import (
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
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

func (p *PaneModule) Screen(id *janet.Value) ([]string, error) {
	defer id.Free()

	pane, err := resolvePane(p.Tree, id)
	if err != nil {
		return nil, err
	}

	state := pane.Screen().State()
	lines := make([]string, len(state.Image))
	for _, line := range state.Image {
		lines = append(lines, line.String())
	}

	return lines, nil
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

	for _, key := range text {
		pane.Screen().Send(taro.KeyMsg{
			Type:  taro.KeyRunes,
			Runes: []rune{key},
		})
	}

	return nil
}
