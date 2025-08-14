package api

import (
	"fmt"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/bind/trie"
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/keys"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
)

type KeyModule struct {
	Tree                              *tree.Tree
	SearchBinds, TimeBinds, CopyBinds *bind.BindScope
}

type regexKey struct {
	_       struct{} `janet:"tuple"`
	Type    janet.Keyword
	Pattern string
}

type countKey struct {
	_       struct{} `janet:"tuple"`
	Type    janet.Keyword
	Pattern *janet.Value // Use *janet.Value to handle any type
	Min     int
	Max     int
}

// parsePattern parses a Janet value that could be either a string or a regex tuple
// Returns the appropriate pattern (string or *trie.Regex) for use in key bindings
func parsePattern(value *janet.Value) (interface{}, error) {
	// Try to unmarshal as string first
	var patternStr string
	if value.Unmarshal(&patternStr) == nil {
		return patternStr, nil
	}
	
	// Try to unmarshal as regex tuple
	var patternRe regexKey
	if value.Unmarshal(&patternRe) == nil {
		regexPattern, err := trie.NewRegex(patternRe.Pattern)
		if err != nil {
			return nil, fmt.Errorf("invalid regex pattern: %w", err)
		}
		return regexPattern, nil
	}
	
	return nil, fmt.Errorf("pattern must be either a string or a regex tuple")
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
	var count countKey
	for i, item := range array {
		strErr := item.Unmarshal(&str)
		if strErr == nil {
			key, ok := keys.FromHuman(str)
			if !ok {
				err = fmt.Errorf(
					"invalid key specifier %s",
					str,
				)
				return
			}

			result = append(result, key.String())
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

		count = countKey{} // Reset the struct
		countErr := item.Unmarshal(&count)
		if countErr == nil {
			// Parse the pattern using centralized logic
			actualPattern, parseErr := parsePattern(count.Pattern)
			if parseErr != nil {
				err = fmt.Errorf("invalid pattern in count at index %d: %w", i, parseErr)
				return
			}
			
			pattern, countErr := trie.NewCount(actualPattern, count.Min, count.Max)
			if countErr != nil {
				err = fmt.Errorf("invalid count pattern at index %d", i)
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

	err = target.Unmarshal(&KEYWORD_SEARCH)
	if err == nil {
		return k.SearchBinds, nil
	}

	return nil, fmt.Errorf(
		"target must be one of :root, :time, :copy, :search, or node ID",
	)
}

type BindParams struct {
	Tag string
}

func (k *KeyModule) Bind(
	target *janet.Value,
	sequence *janet.Value,
	callback *janet.Function,
	bindParams *janet.Named[BindParams],
) error {
	defer target.Free()
	defer sequence.Free()

	params := bindParams.Values()
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
			Tag:      params.Tag,
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
	Tag      string
	Sequence []string
	Function *janet.Value
}

func (k *KeyModule) Get(target *janet.Value) ([]Binding, error) {
	defer target.Free()
	scope, err := k.getScope(target)
	if err != nil {
		return nil, err
	}

	binds := []Binding{}
	for _, leaf := range scope.Leaves() {
		binds = append(
			binds,
			Binding{
				Sequence: leaf.Path,
				Tag:      leaf.Value.Tag,
				Function: leaf.Value.Callback.Value,
			},
		)
	}

	return binds, nil
}

func (k *KeyModule) Current(context interface{}) ([]Binding, error) {
	client, err := getClient(context)
	if err != nil {
		return nil, err
	}

	return client.Binds(), nil
}

