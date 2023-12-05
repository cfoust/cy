package api

import (
	"fmt"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/bind/trie"
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
)

type KeyModule struct {
	Tree        *tree.Tree
	ReplayBinds *bind.BindScope
}

var (
	KEYWORD_ROOT   = janet.Keyword("root")
	KEYWORD_REPLAY = janet.Keyword("replay")

	KEYWORD_RE = janet.Keyword("re")
)

func (k *KeyModule) resolveGroup(target *janet.Value) (*tree.Group, error) {
	// first try keyword
	err := target.Unmarshal(&KEYWORD_ROOT)
	if err == nil {
		return k.Tree.Root(), nil
	}

	// otherwise, node ID
	var id tree.NodeID
	if err := target.Unmarshal(&id); err != nil {
		return nil, err
	}

	group, ok := k.Tree.GroupById(id)
	if !ok {
		return nil, fmt.Errorf("group not found: %d", id)
	}

	return group, nil
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

func (k *KeyModule) Bind(target *janet.Value, sequence *janet.Value, callback *janet.Function) error {
	defer target.Free()
	defer sequence.Free()

	var scope *bind.BindScope
	group, err := k.resolveGroup(target)
	if err == nil {
		scope = group.Binds()
	} else {
		replayErr := target.Unmarshal(&KEYWORD_REPLAY)
		if replayErr != nil {
			return fmt.Errorf("target must be one of :root, :replay, or node ID")
		}

		scope = k.ReplayBinds
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
