package api

import (
	"fmt"

	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
)

type PaneModule struct {
	Tree *tree.Tree
}

func (p *PaneModule) Attach(context interface{}, id *janet.Value) error {
	defer id.Free()

	client, ok := context.(Client)
	if !ok {
		return fmt.Errorf("missing client context")
	}

	pane, err := resolvePane(p.Tree, id)
	if err != nil {
		return err
	}

	return client.Attach(pane)
}

func (p *PaneModule) HistoryNext(context interface{}) error {
	client, ok := context.(Client)
	if !ok {
		return fmt.Errorf("missing client context")
	}

	return client.HistoryNext()
}

func (p *PaneModule) HistoryLast(context interface{}) error {
	client, ok := context.(Client)
	if !ok {
		return fmt.Errorf("missing client context")
	}

	return client.HistoryLast()
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
