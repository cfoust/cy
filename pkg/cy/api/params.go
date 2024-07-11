package api

import (
	"fmt"

	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/params"
)

type ParamModule struct {
	Tree *tree.Tree
}

// haha
type ParamParams struct {
	Target *janet.Value
}

// isClientTarget reports whether value is the :client keyword.
func isClientTarget(value *janet.Value) bool {
	return value.Unmarshal(&KEYWORD_CLIENT) == nil
}

func (p *ParamModule) Get(
	context interface{},
	key *janet.Value,
	named *janet.Named[ParamParams],
) (interface{}, error) {
	defer key.Free()

	var keyword janet.Keyword
	err := key.Unmarshal(&keyword)
	if err != nil {
		return nil, err
	}

	params := named.Values()

	// Need to be careful to always free params.Target
	if params.Target != nil {
		defer params.Target.Free()
	}

	if params.Target == nil || isClientTarget(params.Target) {
		client, ok := context.(Client)
		if !ok {
			return nil, fmt.Errorf("missing client context")
		}

		value, _ := client.Get(string(keyword))
		return value, nil
	}

	node, err := resolveNode(p.Tree, params.Target)
	if err != nil {
		return nil, err
	}

	value, _ := node.Params().Get(string(keyword))
	return value, nil
}

func (p *ParamModule) Set(
	context interface{},
	target *janet.Value,
	key *janet.Value,
	value *janet.Value,
) error {
	defer key.Free()
	defer target.Free()

	var keyword janet.Keyword
	err := key.Unmarshal(&keyword)
	if err != nil {
		return err
	}

	var params *params.Parameters
	if isClientTarget(target) {
		if client, ok := context.(Client); ok {
			params = client.Params()
		} else {
			return fmt.Errorf("missing client context")
		}
	} else {
		node, err := resolveNode(p.Tree, target)
		if err != nil {
			return err
		}
		params = node.Params()
	}

	return params.Set(string(keyword), value)
}
