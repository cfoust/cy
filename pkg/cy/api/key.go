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

type regexStep struct {
	Type    janet.Keyword
	Pattern string
}

type countStep struct {
	Type    janet.Keyword
	Pattern *janet.Value
	Min     int
	Max     int
}

func stepToJanet(step trie.Step) any {
	switch s := step.(type) {
	case *trie.LiteralStep:
		return s.Value
	case *trie.RegexStep:
		return regexStep{
			Type:    KEYWORD_RE,
			Pattern: s.Pattern,
		}
	case *trie.CountStep:
		return struct {
			Type    janet.Keyword
			Pattern any
			Min     int
			Max     int
		}{
			Type:    KEYWORD_COUNT,
			Pattern: stepToJanet(s.Pattern),
			Min:     s.Min,
			Max:     s.Max,
		}
	}
	return nil
}

func StepsToJanet(steps []trie.Step) (result []any) {
	for _, step := range steps {
		result = append(result, stepToJanet(step))
	}
	return
}

func parsePattern(value *janet.Value) (trie.Step, error) {
	var str string
	if value.Unmarshal(&str) == nil {
		key, ok := keys.FromHuman(str)
		if !ok {
			err := fmt.Errorf(
				"invalid key specifier %s",
				str,
			)
			return nil, err
		}

		return trie.NewLiteralStep(key.String()), nil
	}

	patternRe := regexStep{
		Type: KEYWORD_RE,
	}
	if value.Unmarshal(&patternRe) == nil {
		step, err := trie.NewRegexStep(
			patternRe.Pattern,
		)
		if err != nil {
			return nil, fmt.Errorf("invalid regex pattern: %w", err)
		}
		return step, nil
	}

	patternCount := countStep{
		Type: KEYWORD_COUNT,
	}
	if value.Unmarshal(&patternCount) == nil {
		pattern, err := parsePattern(patternCount.Pattern)
		if err != nil {
			return nil, fmt.Errorf(
				"invalid count pattern: %w",
				err,
			)
		}

		return trie.NewCountStep(
			pattern,
			patternCount.Min,
			patternCount.Max,
		), nil
	}

	return nil, fmt.Errorf(
		"step shape not recognized",
	)
}

func getKeySequence(value *janet.Value) (result []trie.Step, err error) {
	var array []*janet.Value
	err = value.Unmarshal(&array)
	if err != nil {
		return
	}

	for i, item := range array {
		var step trie.Step
		step, err = parsePattern(item)
		if err != nil {
			err = fmt.Errorf(
				"invalid pattern at index %d: %w",
				i,
				err,
			)
			return
		}

		result = append(result, step)
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
	Sequence []any
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
				Sequence: StepsToJanet(leaf.Path),
				Tag:      leaf.Value.Tag,
				Function: leaf.Value.Callback.Value,
			},
		)
	}

	return binds, nil
}

func (k *KeyModule) Current(context any) ([]Binding, error) {
	client, err := getClient(context)
	if err != nil {
		return nil, err
	}

	return client.Binds(), nil
}
