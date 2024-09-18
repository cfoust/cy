package api

import (
	_ "embed"
	"fmt"
	"path/filepath"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/search"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"
)

type SearchModule struct {
	Lifetime                          util.Lifetime
	Tree                              *tree.Tree
	SearchBinds, TimeBinds, CopyBinds *bind.BindScope
}

func (m *SearchModule) send(context interface{}, msg taro.Msg) error {
	client, err := getClient(context)
	if err != nil {
		return err
	}

	node := client.Node()
	if node == nil {
		return fmt.Errorf("client was missing node")
	}

	pane, ok := node.(*tree.Pane)
	if !ok {
		return fmt.Errorf("client node was not pane")
	}

	pane.Screen().Send(msg)
	return nil
}

func (m *SearchModule) sendAction(context interface{}, action search.ActionType) error {
	return m.send(context, search.ActionEvent{
		Type: action,
	})
}

func (m *SearchModule) Cancel(context interface{}) error {
	return m.sendAction(context, search.ActionCancel)
}

func (m *SearchModule) Next(context interface{}) error {
	return m.sendAction(context, search.ActionNext)
}

func (m *SearchModule) Prev(context interface{}) error {
	return m.sendAction(context, search.ActionPrev)
}

func (m *SearchModule) First(context interface{}) error {
	return m.sendAction(context, search.ActionFirst)
}

func (m *SearchModule) Last(context interface{}) error {
	return m.sendAction(context, search.ActionLast)
}

func (m *SearchModule) FocusInput(context interface{}) error {
	return m.sendAction(context, search.ActionInput)
}

type SearchParams struct {
	Files   []string
	Workers *int
}

func (m *SearchModule) New(
	groupId *janet.Value,
	query string,
	named *janet.Named[SearchParams],
) (tree.NodeID, error) {
	defer groupId.Free()

	group, err := resolveGroup(m.Tree, groupId)
	if err != nil {
		return 0, err
	}

	numWorkers := group.Params().NumSearchWorkers()

	params := named.Values()
	if params.Workers != nil {
		numWorkers = *params.Workers
	}

	files := params.Files
	if len(files) == 0 {
		files, err = filepath.Glob(
			filepath.Join(
				group.Params().DataDirectory(),
				"*.borg",
			),
		)
		if err != nil {
			return 0, err
		}
	}

	ctx := m.Lifetime.Ctx()
	s := search.New(
		ctx,
		m.SearchBinds,
		m.TimeBinds,
		m.CopyBinds,
		search.WithRequest(search.Request{
			Query:   query,
			Files:   files,
			Workers: numWorkers,
		}),
		search.WithParams(group.Params()),
	)

	pane := group.NewPane(ctx, s)
	return pane.Id(), nil
}
