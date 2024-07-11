package api

import (
	"fmt"

	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
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
	if params.Target == nil || isClientTarget(params.Target) {
		client, ok := context.(Client)
		if !ok {
			return nil, fmt.Errorf("missing client context")
		}

		value, _ := client.Get(string(keyword))
		return value, nil
	}

	defer params.Target.Free()

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

	// If there is no client, this probably means a parameter is being set
	// in cy's startup script.
	var node tree.Node = p.Tree.Root()
	if client, ok := context.(Client); ok {
		node = client.Node()
		if node == nil {
			return fmt.Errorf("client was not attached")
		}
	}

	var keyword janet.Keyword
	err := key.Unmarshal(&keyword)
	if err != nil {
		return err
	}

	node.Params().Set(string(keyword), value)

	return fmt.Errorf("parameter type not supported")
}
