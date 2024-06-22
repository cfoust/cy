package api

import (
	"fmt"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/bind/trie"
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
)

type KeyModule struct {
	Tree                 *tree.Tree
	TimeBinds, CopyBinds *bind.BindScope
}

type regexKey struct {
	_       struct{} `janet:"tuple"`
	Type    janet.Keyword
	Pattern string
}

func getKeySequence(value *janet.Value) (result []interface{}, err error) {
	var array []*janet.Value
	err = value.Unmarshal(&array)
	if err != nil {
		return
	}

	var str string
	re := regexKey{
		Type: KEYWORD_RE,
	}
	for i, item := range array {
		strErr := item.Unmarshal(&str)
		if strErr == nil {
			result = append(result, str)
			continue
		}

		reErr := item.Unmarshal(&re)
		if reErr == nil {
			pattern, reErr := trie.NewRegex(re.Pattern)
			if reErr != nil {
				err = fmt.Errorf("invalid regex at index %d", i)
				return
			}
			result = append(result, pattern)
			continue
		}

		err = fmt.Errorf("invalid key at index %d", i)
		return
	}

	return
}

func (k *KeyModule) getScope(target *janet.Value) (*bind.BindScope, error) {
	node, err := resolveNode(k.Tree, target)
	if err == nil {
		return node.Binds(), nil
	}

	err = target.Unmarshal(&KEYWORD_TIME)
	if err == nil {
		return k.TimeBinds, nil
	}

	err = target.Unmarshal(&KEYWORD_COPY)
	if err == nil {
		return k.CopyBinds, nil
	}

	return nil, fmt.Errorf("target must be one of :root, :time, :copy, or node ID")
}

func (k *KeyModule) Bind(target *janet.Value, sequence *janet.Value, callback *janet.Function) error {
	defer target.Free()
	defer sequence.Free()

	scope, err := k.getScope(target)
	if err != nil {
		return err
	}

	translated, err := getKeySequence(sequence)
	if err != nil {
		return err
	}

	scope.Set(
		translated,
		bind.Action{
			Callback: callback,
		},
	)

	return nil
}

func (k *KeyModule) Unbind(target *janet.Value, sequence *janet.Value) error {
	defer target.Free()
	defer sequence.Free()

	scope, err := k.getScope(target)
	if err != nil {
		return err
	}

	translated, err := getKeySequence(sequence)
	if err != nil {
		return err
	}

	scope.Clear(translated)

	return nil
}

func (k *KeyModule) Remap(target *janet.Value, from, to *janet.Value) error {
	defer target.Free()
	defer from.Free()
	defer to.Free()

	scope, err := k.getScope(target)
	if err != nil {
		return err
	}

	fromSequence, err := getKeySequence(from)
	if err != nil {
		return err
	}

	toSequence, err := getKeySequence(to)
	if err != nil {
		return err
	}

	scope.Remap(fromSequence, toSequence)

	return nil
}

type Binding struct {
	Node     tree.NodeID
	Sequence []string
	Function *janet.Value
}

func NewBinding(node tree.Node, leaf trie.Leaf[bind.Action]) Binding {
	return Binding{
		Node:     node.Id(),
		Sequence: leaf.Path,
		Function: leaf.Value.Callback.Value,
	}
}

func (k *KeyModule) Get(target *janet.Value) ([]Binding, error) {
	defer target.Free()
	node, err := resolveNode(k.Tree, target)
	if err != nil {
		return nil, err
	}

	binds := []Binding{}
	for _, leaf := range node.Binds().Leaves() {
		binds = append(
			binds,
			NewBinding(node, leaf),
		)
	}

	return binds, nil
}

func (k *KeyModule) Current(user interface{}) []Binding {
	client, ok := user.(Client)
	if !ok {
		return nil
	}

	return client.Binds()
}
